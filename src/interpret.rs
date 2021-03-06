use crate::ast::*;
use crate::intern::Interner;
use crate::vm::Value;

use std::collections::HashMap;
use std::rc::Rc;

// An interned string.
// We rarely need the actual value of this, but
// it can be useful to keep around.
type Var = u32;

// Variable environments
struct Environment {
  envs: Vec<HashMap<Var, Rc<Value<Var>>>>,
}

impl Environment {
  // Todo: first class functions, global variables
  fn new() -> Self {
    return Self {
      envs: vec![HashMap::new()],
    };
  }

  fn def(&mut self, name: Var, value: Rc<Value<Var>>) {
    let env = self.envs.last_mut().unwrap();
    match env.get_mut(&name) {
      None => {
        env.insert(name, value);
      }
      Some(entry) => {
        *entry = value;
      }
    }
  }

  fn get(&self, name: Var) -> Option<Rc<Value<Var>>> {
    for env in self.envs.iter().rev() {
      if let Some(value) = env.get(&name) {
        return Some(value.clone());
      }
    }
    None
  }

  fn get_mut(&mut self, name: &Var) -> Option<&mut Rc<Value<Var>>> {
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

struct Function(Vec<Var>, Expr<Var>);

// Context of functions
struct Context {
  intern: Interner,
  functions: HashMap<Var, Rc<Function>>,
  env: Environment,
}

impl Context {
  fn new(intern: Interner, program: Program<Var>) -> Self {
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
      intern,
      functions,
      env: Environment::new(),
    };
  }

  // What we want... is to get a mutable reference to the RC targetted by
  // the lvalue. This is the value we will try to modify, and if we can't
  // modify it, we will mutate this instance of it, splitting out from
  // all the rest.
  fn mutable_access<'a>(
    intern: &Interner,
    value: &'a mut Rc<Value<Var>>,
    name: &Var,
  ) -> &'a mut Rc<Value<Var>> {
    // Todo: closer investigation around the mutability
    // here. How often does it really copy? Does the hashmap's
    // pair always force a clone? Can we get a reference into the map?
    match Rc::make_mut(value) {
      Value::Tuple(fields) => {
        // Todo: make this less stupid by enforcing that numerical
        // indices actually get their corresponding u32, and so can
        // be used to index directly.
        let i = intern.lookup(name).parse::<usize>().unwrap();
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

  fn access<'a>(intern: &Interner, value: Rc<Value<Var>>, name: &Var) -> Rc<Value<Var>> {
    match value.as_ref() {
      Value::Tuple(fields) => {
        let i = intern.lookup(name).parse::<usize>().unwrap();
        return fields[i].clone();
      }
      Value::Struct(fields) => {
        for (f, val) in fields.iter() {
          if f == name {
            return val.clone();
          }
        }
        panic!("Unknown member")
      }
      _ => panic!("Non-structured access"),
    }
  }

  fn eval_lvalue<'a>(&'a mut self, lval: &LValue<Var>) -> &mut Rc<Value<Var>> {
    match lval {
      LValue::Ident(v) => self.env.get_mut(v).expect("Undefined variable"),
      LValue::Access(vars) => match &vars[..] {
        [] => panic!("Empty access"),
        [v, vs @ ..] => {
          let mut cell = self.env.get_mut(v).expect("Undefined variable");
          for v in vs.iter() {
            cell = Context::mutable_access(&self.intern, cell, v);
          }
          cell
        }
      },
    }
  }

  fn eval_stmt(&mut self, stmt: &Stmt<Var>) -> Option<Rc<Value<Var>>> {
    use Stmt::*;
    match stmt {
      Let { name, value } => {
        let result = self.eval_expr(value);
        self.env.def(*name, result);
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

  fn eval_binop(op: BinOp, lhs: Rc<Value<Var>>, rhs: Rc<Value<Var>>) -> Value<Var> {
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

  fn eval_unop(op: UnOp, rhs: Rc<Value<Var>>) -> Value<Var> {
    let rhs = rhs.as_ref().clone();
    match op {
      UnOp::Not => !rhs,
      UnOp::Sub => -rhs,
    }
  }

  // What if we don't want to allocate for "just a dumb int"?
  // Soln: Custom value container. It might be an Rc, it might not.
  fn eval_expr(&mut self, expr: &Expr<Var>) -> Rc<Value<Var>> {
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
          .map(|(f, e)| (*f, self.eval_expr(e)))
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
      Variable(name) => match self.env.get(*name) {
        Some(value) => value,
        None => {
          eprintln!("undefined variable: {}", name);
          panic!()
        }
      },
      FieldAccess { expr, fields } => {
        let mut lhs = self.eval_expr(expr);
        for field in fields.iter() {
          lhs = Context::access(&self.intern, lhs, field)
        }
        lhs
      }
      Call { function, args } => {
        let args = args
          .iter()
          .map(|a| self.eval_expr(a).as_ref().clone())
          .collect();
        self.eval_fn(*function, args)
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

  fn eval_fn(&mut self, name: Var, args: Vec<Value<Var>>) -> Rc<Value<Var>> {
    let func = self.functions.get(&name).expect("fn").clone();
    let (arg_names, body) = (
      &func.0,
      match &func.1 {
        Expr::Block(box body) => &body,
        other => other,
      },
    );
    self.env.push();
    for (name, arg) in arg_names.iter().zip(args.into_iter()) {
      self.env.def(*name, Rc::new(arg));
    }
    let result = self.eval_expr(body);
    self.env.pop();
    result
  }
}

pub fn eval(mut intern: Interner, program: Program<Var>) -> Value<Var> {
  let main = intern.intern("main".to_string());
  let mut ctx = Context::new(intern, program);
  ctx.eval_fn(main, vec![]).as_ref().clone()
}
