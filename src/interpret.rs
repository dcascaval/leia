use crate::ast::*;
use crate::vm::Value;

use std::collections::HashMap;
use std::rc::Rc;

// Variable environments
struct Environment {
  envs: Vec<HashMap<Var, Rc<Value>>>,
}

impl Environment {
  // Todo: first class functions, global variables
  fn new() -> Self {
    return Self {
      envs: vec![HashMap::new()],
    };
  }

  fn def(&mut self, name: &Var, value: Rc<Value>) {
    let env = self.envs.last_mut().unwrap();
    if let None = env.get(name) {
      env.insert(name.clone(), value);
    }
  }

  fn get(&self, name: &Var) -> Option<Rc<Value>> {
    for env in self.envs.iter().rev() {
      if let Some(value) = env.get(name) {
        return Some(value.clone());
      }
    }
    None
  }

  fn get_mut(&mut self, name: &Var) -> Option<&mut Rc<Value>> {
    for env in self.envs.iter_mut().rev() {
      if let Some(value) = env.get_mut(name) {
        return Some(value);
      }
    }
    None
  }

  fn push(&mut self) {
    self.envs.push(HashMap::new())
  }

  fn pop(&mut self) {
    self.envs.pop();
  }
}

struct Function(Vec<Var>, Expr);

// Context of functions
struct Context {
  functions: HashMap<Var, Rc<Function>>,
  env: Environment,
}

impl Context {
  fn new(program: Program) -> Self {
    let mut functions = HashMap::new();

    for gstmt in program.0.into_iter() {
      if let Gstmt::Function {
        name, args, body, ..
      } = gstmt
      {
        let vars = args.into_iter().map(|(_, b)| b).collect();
        functions.insert(name, Rc::new(Function(vars, body)));
      }
    }

    return Self {
      functions,
      env: Environment::new(),
    };
  }

  // What we want... is to get a mutable reference to the RC targetted by
  // the lvalue. This is the value we will try to modify, and if we can't
  // modify it, we will mutate this instance of it, splitting out from
  // all the rest.
  fn access<'a>(value: &'a mut Rc<Value>, name: &Var) -> &'a mut Rc<Value> {
    // Todo: closer investigation around the mutability
    // here. How often does it really copy? Does the hashmap's
    // pair always force a clone? Can we get a reference into the map?
    match Rc::make_mut(value) {
      Value::Tuple(fields) => {
        let i = name.parse::<usize>().unwrap();
        return &mut fields[i];
      }
      Value::Struct(fields) => {
        for (f, val) in fields.iter_mut() {
          if f == name {
            return val;
          }
        }
        panic!("Unknown member")
      }
      _ => panic!("Non-structured access"),
    }
  }

  fn eval_lvalue<'a>(&'a mut self, lval: &LValue) -> &mut Rc<Value> {
    match lval {
      LValue::Ident(v) => self.env.get_mut(v).expect("Undefined variable"),
      LValue::Access(vars) => match &vars[..] {
        [] => panic!("Empty access"),
        [v, vs @ ..] => {
          let mut cell = self.env.get_mut(v).expect("Undefined variable");
          for v in vs.iter() {
            cell = Context::access(cell, v);
          }
          cell
        }
      },
    }
  }

  fn eval_stmt(&mut self, stmt: &Stmt) -> Option<Rc<Value>> {
    use Stmt::*;
    match stmt {
      Let { name, value } => {
        let result = self.eval_expr(value);
        self.env.def(name, result);
        None
      }
      Assign { value, target } => {
        let result = self.eval_expr(value);
        let target = self.eval_lvalue(target);
        *target = result;
        None
      }
      Expr(e) => Some(self.eval_expr(e)),
      Loop(_) => todo!(),
      Return(_) => todo!(),
      BREAK => todo!(),
    }
  }

  fn eval_binop(op: BinOp, lhs: Rc<Value>, rhs: Rc<Value>) -> Value {
    let (lhs, rhs) = (lhs.as_ref().clone(), rhs.as_ref().clone());
    match op {
      BinOp::Add => lhs + rhs,
      BinOp::Sub => lhs - rhs,
      BinOp::Mul => lhs * rhs,
      BinOp::Div => lhs / rhs,
      BinOp::Mod => lhs % rhs,
      BinOp::Lt => lhs.lt(rhs),
      BinOp::Leq => lhs.lte(rhs),
      BinOp::Gt => lhs.gt(rhs),
      BinOp::Geq => lhs.gte(rhs),
      BinOp::Eql => lhs.eq(rhs),
      BinOp::Neq => lhs.neq(rhs),
      BinOp::And => lhs.and(rhs),
      BinOp::Or => lhs.or(rhs),
    }
  }

  fn eval_unop(op: UnOp, rhs: Rc<Value>) -> Value {
    let rhs = rhs.as_ref().clone();
    match op {
      UnOp::Not => !rhs,
      UnOp::Sub => -rhs,
    }
  }

  // What if we don't want to allocate for "just a dumb int"?
  // Soln: Custom value container. It might be an Rc, it might not.
  fn eval_expr(&mut self, expr: &Expr) -> Rc<Value> {
    use Expr::*;

    match expr {
      Block(box expr) => {
        self.env.push();
        let result = self.eval_expr(&expr);
        self.env.pop();
        result
      }
      Statements(stmts) => {
        if let Some((last, stmts)) = stmts[..].split_last() {
          for stmt in stmts.iter() {
            self.eval_stmt(stmt);
          }
          if let Some(v) = self.eval_stmt(last) {
            return v;
          }
        }
        Rc::new(Value::Unit)
      }
      IntLiteral(i) => Rc::new(Value::Int(*i)),
      BoolLiteral(b) => Rc::new(Value::Bool(*b)),
      FloatLiteral(f) => Rc::new(Value::Float(*f)),
      StructLiteral { fields, .. } => Rc::new(Value::Struct(
        fields
          .iter()
          .map(|(f, e)| (f.clone(), self.eval_expr(e)))
          .collect(),
      )),
      TupleLiteral(fields) => Rc::new(Value::Tuple(
        fields.iter().map(|v| self.eval_expr(v)).collect(),
      )),
      AsExpression { .. } => todo!(),
      EnumLiteral { .. } => todo!(),
      WithExpression { expr, fields } => {
        let v = self.eval_expr(expr);
        // We copy all fields in a with-expression (or at least, their names --
        // we clone their RCs and later replace what we need.
        let mut old_fields = match &*v {
          Value::Struct(old_fields) => old_fields.clone(),
          _ => {
            eprintln!("With on non-structure");
            panic!()
          }
        };
        for (field, expr) in fields.iter() {
          let new_value = self.eval_expr(expr);
          for (f, v) in old_fields.iter_mut() {
            if f == field {
              *v = new_value;
              break;
            }
          }
        }
        Rc::new(Value::Struct(old_fields))
      }
      BinaryOp { op, lhs, rhs } => {
        let (lhs, rhs) = (self.eval_expr(lhs), self.eval_expr(rhs));
        Rc::new(Context::eval_binop(*op, lhs, rhs))
      }
      UnaryOp { op, rhs } => {
        let rhs = self.eval_expr(rhs);
        Rc::new(Context::eval_unop(*op, rhs))
      }
      Variable(name) => match self.env.get(name) {
        Some(value) => value,
        None => {
          eprintln!("undefined variable: {}", name);
          panic!()
        }
      },

      // Todo: This is where our copy-on-write really shines.
      FieldAccess { .. } => {
        // let mut e = expr;
        todo!()
      }
      Call { function, args } => {
        let args = args
          .iter()
          .map(|a| self.eval_expr(a).as_ref().clone())
          .collect();
        self.eval_fn(function, args)
      }
      If { condition, t1, t2 } => {
        if let Value::Bool(b) = &*self.eval_expr(condition) {
          if *b {
            self.eval_expr(t1)
          } else {
            self.eval_expr(t2)
          }
        } else {
          eprintln!("Non-boolean condition");
          panic!()
        }
      }
      Match(_) => todo!(),
    }
  }

  fn eval_fn(&mut self, name: &Var, args: Vec<Value>) -> Rc<Value> {
    let func = self.functions.get(name).expect("fn").clone();
    let (arg_names, body) = (
      &func.0,
      match &func.1 {
        Expr::Block(box body) => &body,
        other => other,
      },
    );
    self.env.push();
    for (name, arg) in arg_names.iter().zip(args.into_iter()) {
      self.env.def(name, Rc::new(arg));
    }
    let result = self.eval_expr(body);
    self.env.pop();
    result
  }
}

pub fn eval(program: Program) -> Value {
  let mut ctx = Context::new(program);
  ctx.eval_fn(&String::from("main"), vec![]).as_ref().clone()
}
