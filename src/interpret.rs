use crate::ast::*;
use crate::vm::Value;

use std::borrow::Cow;
use std::collections::HashMap;
use std::rc::Rc;

// Variable environments
struct Environment {
  envs: Vec<HashMap<Var, Value>>,
}

impl Environment {
  // Todo: first class functions, global variables
  fn new() -> Self {
    return Self {
      envs: vec![HashMap::new()],
    };
  }

  fn def(&mut self, name: &Var, value: &Value) {
    let env = self.envs.last_mut().unwrap();
    if let None = env.get(name) {
      env.insert(name.clone(), value.clone());
    }
  }

  fn get(&self, name: &Var) -> Option<&Value> {
    for env in self.envs.iter().rev() {
      if let Some(value) = env.get(name) {
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

  fn eval_stmt(&mut self, _stmt: &Stmt) -> Cow<Value> {
    todo!()
  }

  fn eval_binop(_op: BinOp, _lhs: Value, _rhs: Value) -> Value {
    todo!()
  }

  fn eval_unop(_op: UnOp, _rhs: Value) -> Value {
    todo!()
  }

  fn eval_expr(&mut self, expr: &Expr) -> Cow<Value> {
    use Expr::*;

    match expr {
      Block(box expr) => {
        self.env.push();
        let result = self.eval_expr(&expr).into_owned();
        self.env.pop();
        Cow::Owned(result)
      }
      Statements(stmts) => {
        if let Some((last, stmts)) = stmts[..].split_last() {
          for stmt in stmts.iter() {
            self.eval_stmt(stmt);
          }
          self.eval_stmt(last)
        } else {
          Cow::Owned(Value::Unit)
        }
      }
      IntLiteral(i) => Cow::Owned(Value::Int(*i)),
      BoolLiteral(b) => Cow::Owned(Value::Bool(*b)),
      FloatLiteral(f) => Cow::Owned(Value::Float(*f)),
      StructLiteral { fields, .. } => Cow::Owned(Value::Struct(
        fields
          .iter()
          .map(|(f, e)| (f.clone(), self.eval_expr(e).into_owned()))
          .collect(),
      )),
      TupleLiteral(fields) => Cow::Owned(Value::Tuple(
        fields
          .iter()
          .map(|v| self.eval_expr(v).into_owned())
          .collect(),
      )),
      AsExpression { .. } => todo!(),
      EnumLiteral { .. } => todo!(),
      WithExpression { expr, fields } => {
        let v = self.eval_expr(expr);
        let mut old_fields = match v {
          Cow::Owned(Value::Struct(old_fields)) => old_fields,
          Cow::Borrowed(Value::Struct(old_fields)) => old_fields.clone(),
          _ => {
            eprintln!("With on non-structure");
            panic!()
          }
        };
        for (field, expr) in fields.iter() {
          let new_value = self.eval_expr(expr).into_owned();
          for (f, v) in old_fields.iter_mut() {
            if f == field {
              *v = new_value;
              break;
            }
          }
        }
        Cow::Owned(Value::Struct(old_fields))
      }
      BinaryOp { op, lhs, rhs } => {
        let (lhs, rhs) = (
          self.eval_expr(lhs).into_owned(),
          self.eval_expr(rhs).into_owned(),
        );
        Cow::Owned(Context::eval_binop(*op, lhs, rhs))
      }
      UnaryOp { op, rhs } => {
        let rhs = self.eval_expr(rhs).into_owned();
        Cow::Owned(Context::eval_unop(*op, rhs))
      }
      Variable(name) => match self.env.get(name) {
        Some(value) => Cow::Borrowed(value),
        None => {
          eprintln!("undefined variable: {}", name);
          panic!()
        }
      },

      // Todo: This is where cow really shines. However our acess type sucks
      // right now. It should be one at a time.
      FieldAccess { .. } => {
        // let mut e = expr;
        todo!()
      }
      Call { function, args } => {
        let args = args
          .iter()
          .map(|a| self.eval_expr(a).into_owned())
          .collect();
        self.eval_fn(function, args)
      }
      If { condition, t1, t2 } => {
        let b = match self.eval_expr(condition) {
          Cow::Borrowed(Value::Bool(b)) => *b,
          Cow::Owned(Value::Bool(b)) => b,
          _ => {
            eprintln!("Non-boolean condition");
            panic!()
          }
        };
        if b {
          self.eval_expr(t1)
        } else {
          self.eval_expr(t2)
        }
      }
      Match(_) => todo!(),
    }
  }

  fn eval_fn(&mut self, name: &Var, args: Vec<Value>) -> Cow<Value> {
    let func = self.functions.get(name).expect("fn").clone();
    let (arg_names, body) = (&func.0, &func.1);
    self.env.push();
    for (i, name) in arg_names.iter().enumerate() {
      self.env.def(name, &args[i]);
    }
    let result = self.eval_expr(body).into_owned();
    self.env.pop();
    Cow::Owned(result)
  }
}

pub fn eval(program: Program) -> Value {
  let mut ctx = Context::new(program);
  ctx.eval_fn(&String::from("main"), vec![]).into_owned()
}
