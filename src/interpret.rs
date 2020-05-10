use crate::ast::*;
use crate::vm::Value;

use std::collections::HashMap;

// Variable environments
struct Environment<'a> {
  envs: Vec<HashMap<&'a str, Value>>,
}

impl<'a> Environment<'a> {
  // Todo: first class functions, global variables
  fn new() -> Self {
    return Self {
      envs: vec![HashMap::new()],
    };
  }

  fn def(&mut self, name: &'a str, value: &Value) {
    let env = self.envs.last_mut().unwrap();
    if let None = env.get(name) {
      env.insert(name, value.clone());
    }
  }

  fn get(&mut self, name: &str) -> Option<Value> {
    for env in self.envs.iter_mut().rev() {
      if let Some(value) = env.get(name) {
        return Some(value.clone());
      }
    }
    None
  }
}

type Function = (Vec<String>, Expr);

// Context of functions
struct Context<'b> {
  functions: HashMap<Var, Function>,
  envs: Environment<'b>,
}

impl<'a> Context<'a> {
  fn new(program: Program) -> Self {
    let mut functions = HashMap::new();
    for gstmt in program.0.into_iter() {
      if let Gstmt::Function {
        name, args, body, ..
      } = gstmt
      {
        let vars = args.into_iter().map(|(_, b)| b).collect();
        functions.insert(name, (vars, body));
      }
    }
    return Self {
      functions,
      envs: Environment::new(),
    };
  }

  fn call_function(&mut self, _name: &str) -> Value {
    todo!()
  }
}

pub fn eval(program: Program) -> Value {
  let mut ctx = Context::new(program);
  ctx.call_function("main".into())
}
