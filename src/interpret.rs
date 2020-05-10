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

  fn assign(&mut self, name: &Var, value: Rc<Value>) {
    for env in self.envs.iter_mut().rev() {
      if let Some(entry) = env.get_mut(name) {
        *entry = value;
        return;
      }
    }
    eprintln!("Cannot assign {} to undefined variable: {}", *value, name);
    panic!()
  }

  fn get(&self, name: &Var) -> Option<Rc<Value>> {
    for env in self.envs.iter().rev() {
      if let Some(value) = env.get(name) {
        return Some(value.clone());
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

  fn eval_stmt(&mut self, stmt: &Stmt) -> Option<Rc<Value>> {
    use Stmt::*;
    match stmt {
      Let { name, value } => {
        let result = self.eval_expr(value);
        self.env.def(name, result);
        None
      }
      Assign { value, .. } => {
        let _result = self.eval_expr(value);
        // Mutating stuff... urgh.
        // Not that easy.
        // self.env.assign(target, result);
        None
      }
      Expr(_) => todo!(),
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
