use crate::ast;
use std::collections::HashMap;

pub struct Interner {
  counter: u32,
  mapping: HashMap<ast::Var, u32>,
}

impl Interner {
  pub fn new() -> Self {
    Self {
      counter: 0,
      mapping: HashMap::new(),
    }
  }

  pub fn lookup(&self, _index: &u32) -> &ast::Var {
    todo!()
  }

  pub fn intern(&mut self, var: ast::Var) -> u32 {
    match self.mapping.get(&var) {
      Some(v) => *v,
      None => {
        let result = self.counter;
        self.counter += 1;
        self.mapping.insert(var, result);
        result
      }
    }
  }
}

pub fn convert(_program: ast::Program<String>) -> (Interner, ast::Program<u32>) {
  (Interner::new(), ast::Program(Vec::new()))
}
