use crate::ast::*;
use crate::vm::Value;

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

  fn get(&self, name: Var) -> Option<Value> {
    for env in self.envs.iter().rev() {
      if let Some(value) = env.get(&name) {
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

  fn eval_expr(&mut self, expr: &Expr) -> Value {
    todo!()
  }

  fn eval_fn(&mut self, name: &Var, args: Vec<Value>) -> Value {
    let func = self.functions.get(name).expect("fn").clone();
    let (arg_names, body) = (&func.0, &func.1);
    self.env.push();
    for (i, name) in arg_names.iter().enumerate() {
      self.env.def(name, &args[i]);
    }
    let result = self.eval_expr(body);
    self.env.pop();
    result
  }
}

pub fn eval(program: Program) -> Value {
  let mut ctx = Context::new(program);
  ctx.eval_fn(&String::from("main"), vec![])
}
