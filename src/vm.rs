use crate::ast::*;

use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
  Unit,
  Int(i64),
  Bool(bool),
  Float(f64),
  Tuple(Vec<Rc<Value>>),
  Struct(Vec<(String, Rc<Value>)>),
}

impl Display for Value {
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
    impl std::ops::$name for Value {
      type Output = Value;
      fn $func(self, rhs: Value) -> Self::Output {
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

impl std::cmp::PartialOrd for Value {
  fn partial_cmp(&self, other: &Value) -> Option<std::cmp::Ordering> {
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

impl std::ops::Neg for Value {
  type Output = Value;
  fn neg(self) -> Self::Output {
    match self {
      Value::Float(f) => Value::Float(-f),
      Value::Int(i) => Value::Int(-i),
      _ => panic!("Non-arithmetic neg!"),
    }
  }
}

impl std::ops::Not for Value {
  type Output = Value;
  fn not(self) -> Self::Output {
    match self {
      Value::Bool(b) => Value::Bool(!b),
      _ => panic!("Non-boolean not!"),
    }
  }
}

impl Value {
  pub fn lt(self, other: Value) -> Value {
    Value::Bool(self < other)
  }
  pub fn lte(self, other: Value) -> Value {
    Value::Bool(self <= other)
  }
  pub fn gt(self, other: Value) -> Value {
    Value::Bool(self > other)
  }
  pub fn gte(self, other: Value) -> Value {
    Value::Bool(self >= other)
  }
  pub fn eq(self, other: Value) -> Value {
    Value::Bool(self.equals(other))
  }
  pub fn neq(self, other: Value) -> Value {
    Value::Bool(!self.equals(other))
  }
  // implement!
  fn equals(self, _other: Value) -> bool {
    todo!()
  }

  pub fn and(self, other: Value) -> Value {
    match (self, other) {
      (Value::Bool(s), Value::Bool(o)) => Value::Bool(s && o),
      _ => panic!("Non-boolean &&"),
    }
  }

  pub fn or(self, other: Value) -> Value {
    match (self, other) {
      (Value::Bool(s), Value::Bool(o)) => Value::Bool(s || o),
      _ => panic!("Non-boolean &&"),
    }
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
