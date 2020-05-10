struct Interner<'a> {
  counter: usize,
  data: Vec<String>,
  mapping: HashMap<&'a ast::Var, Var>,
}

impl<'a> Interner<'a> {
  fn new() -> Self {
    Self {
      counter: 0,
      data: Vec::new(),
      mapping: HashMap::new(),
    }
  }

  fn intern(&mut self, var: &'a ast::Var) -> usize {
    match self.mapping.get(var) {
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
