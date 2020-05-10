use crate::ast::*;

use std::collections::HashMap;
use std::fmt::Display;

#[derive(Clone, Debug)]
pub enum Value {
  Int(i64),
  Bool(bool),
  Tuple(Vec<Value>),
  Struct(Vec<(String, Value)>),
}

impl Display for Value {
  fn fmt(&self, _f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    todo!()
  }
}

struct VM {
  variables: HashMap<String, Value>,
}

fn eval_stmt(_stmt: Stmt) -> Value {
  todo!()
}
fn eval(_program: Program) -> Value {
  todo!()
}
