use std::fmt::Display;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Value<Var: Display> {
  Unit,
  Int(i64),
  Bool(bool),
  Float(f64),
  Tuple(Vec<Rc<Value<Var>>>),
  Struct(Vec<(Var, Rc<Value<Var>>)>),
}

impl<Var: Display> Display for Value<Var> {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Value::Unit => write!(f, "()"),
      Value::Int(i) => write!(f, "{}", i),
      Value::Bool(b) => write!(f, "{}", b),
      Value::Float(n) => write!(f, "{}", n),
      Value::Tuple(t) => {
        write!(f, "(")?;
        if let [elems @ .., last] = &t[..] {
          for elem in elems.iter() {
            write!(f, "{},", **elem)?;
          }
          write!(f, "{}", **last)?;
        }
        write!(f, ")")
      }
      Value::Struct(fs) => {
        write!(f, "{{")?;
        if let [elems @ .., last] = &fs[..] {
          for (field, var) in elems.iter() {
            write!(f, "{} : {}, ", field, **var)?;
          }
          write!(f, "{} : {} ", last.0, *last.1)?;
        }
        write!(f, "}}")
      }
    }
  }
}

macro_rules! impl_arith {
  ($name:ident, $func:ident, $op: tt) => {
    impl<Var: Display + PartialEq> std::ops::$name for Value<Var> {
      type Output = Value<Var>;
      fn $func(self, rhs: Value<Var>) -> Self::Output {
        match (self, rhs) {
          (Value::Int(i), Value::Int(j)) => Value::Int(i $op j),
          (Value::Float(i), Value::Float(j)) => Value::Float(i $op j),
          (Value::Float(i), Value::Int(j)) => Value::Float(i $op j as f64),
          (Value::Int(i), Value::Float(j)) => Value::Float(i as f64 $op j),
          _ => panic!("Non-arithmetic {}!",stringify!($func)),
        }
      }
    }
  };
}

impl<Var: Display + PartialEq> std::cmp::PartialOrd for Value<Var> {
  fn partial_cmp(&self, other: &Value<Var>) -> Option<std::cmp::Ordering> {
    match (self, other) {
      (Value::Int(i), Value::Int(j)) => i.partial_cmp(j),
      (Value::Float(i), Value::Float(j)) => i.partial_cmp(j),
      (Value::Float(i), Value::Int(j)) => i.partial_cmp(&(*j as f64)),
      (Value::Int(i), Value::Float(j)) => (*i as f64).partial_cmp(j),
      _ => None,
    }
  }
}

impl_arith!(Add,add,+);
impl_arith!(Sub,sub,-);
impl_arith!(Mul,mul,*);
impl_arith!(Div,div,/);
impl_arith!(Rem,rem,%);

impl<Var: Display + PartialEq> std::ops::Neg for Value<Var> {
  type Output = Value<Var>;
  fn neg(self) -> Self::Output {
    match self {
      Value::Float(f) => Value::Float(-f),
      Value::Int(i) => Value::Int(-i),
      _ => panic!("Non-arithmetic neg!"),
    }
  }
}

impl<Var: Display + PartialEq> std::ops::Not for Value<Var> {
  type Output = Value<Var>;
  fn not(self) -> Self::Output {
    match self {
      Value::Bool(b) => Value::Bool(!b),
      _ => panic!("Non-boolean not!"),
    }
  }
}

impl<Var: Display + PartialEq> Value<Var> {
  pub fn lt(self, other: Value<Var>) -> Value<Var> {
    Value::Bool(self < other)
  }
  pub fn lte(self, other: Value<Var>) -> Value<Var> {
    Value::Bool(self <= other)
  }
  pub fn gt(self, other: Value<Var>) -> Value<Var> {
    Value::Bool(self > other)
  }
  pub fn gte(self, other: Value<Var>) -> Value<Var> {
    Value::Bool(self >= other)
  }
  pub fn eq(self, other: Value<Var>) -> Value<Var> {
    Value::Bool(self.equals(other))
  }
  pub fn neq(self, other: Value<Var>) -> Value<Var> {
    Value::Bool(!self.equals(other))
  }
  // implement!
  fn equals(self, _other: Value<Var>) -> bool {
    todo!()
  }

  pub fn and(self, other: Value<Var>) -> Value<Var> {
    match (self, other) {
      (Value::Bool(s), Value::Bool(o)) => Value::Bool(s && o),
      _ => panic!("Non-boolean &&"),
    }
  }

  pub fn or(self, other: Value<Var>) -> Value<Var> {
    match (self, other) {
      (Value::Bool(s), Value::Bool(o)) => Value::Bool(s || o),
      _ => panic!("Non-boolean &&"),
    }
  }
}
