use crate::error::{err, Result};
use std::fmt::Debug;

pub type Var = String;

// Todo: check eq impl
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Typ {
  Unit,
  Int,
  Bool,
  Float,
  Alias(String),
  Composite(Box<Typ>, String),
  Struct(Vec<(String, Typ)>),
  Enum(Vec<(String, Typ)>),
  Tuple(Vec<Typ>),
  Function(Box<Typ>, Box<Typ>),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
  Mul,
  Div,
  Mod,
  Add,
  Sub,
  Lt,
  Leq,
  Gt,
  Geq,
  Eql,
  Neq,
  And,
  Or,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnOp {
  Sub,
  Not,
}

// Program, and defined intrinsics for it.

#[derive(Debug)]
pub struct Program<Var: Debug>(pub Vec<Gstmt<Var>>);

pub type Args<T> = Vec<(Typ, T)>;

#[derive(Debug)]
pub enum Gstmt<Var: Debug> {
  Typedef {
    name: Var,
    typ: Typ,
  },
  Function {
    typ: Typ,
    name: Var,
    args: Args<Var>,
    body: Expr<Var>,
  },
}

#[derive(Debug, Clone)]
pub enum LValue<Var: Debug> {
  Ident(Var),
  Access(Vec<Var>),
}

// Todo: make as structs?
#[derive(Debug, Clone)]
pub enum Stmt<Var: Debug> {
  Let {
    name: Var,
    value: Expr<Var>,
  },
  Assign {
    target: LValue<Var>,
    value: Expr<Var>,
  },
  Expr(Expr<Var>),
  Loop(Expr<Var>),
  Return(Expr<Var>),
  BREAK,
}

#[derive(Debug, Clone)]
pub enum Pattern<Var: Debug> {
  IntLiteral(i64),
  FloatLiteral(f64),
  BooleanLiteral(bool),
  IntRange { min: i64, max: i64 },
  FloatRange { min: f64, max: f64 },
  Struct(Vec<(Var, Pattern<Var>)>),
  Enum { name: Var, args: Vec<Pattern<Var>> },
  Variable(Var),
}

#[derive(Debug, Clone)]
pub enum Expr<Var: Debug> {
  Block(Box<Expr<Var>>),
  Statements(Vec<Stmt<Var>>),
  IntLiteral(i64),
  BoolLiteral(bool),
  FloatLiteral(f64),
  StructLiteral {
    name: Var,
    fields: Vec<(Var, Expr<Var>)>,
  },
  EnumLiteral {
    // Like a function call!
    name: Var,
    args: Vec<Expr<Var>>,
  },
  TupleLiteral(Vec<Expr<Var>>),
  AsExpression {
    expr: Box<Expr<Var>>,
    target: Typ,
  },
  WithExpression {
    expr: Box<Expr<Var>>,
    fields: Vec<(Var, Expr<Var>)>,
  },
  BinaryOp {
    op: BinOp,
    lhs: Box<Expr<Var>>,
    rhs: Box<Expr<Var>>,
  },
  UnaryOp {
    op: UnOp,
    rhs: Box<Expr<Var>>,
  },
  Variable(Var),
  FieldAccess {
    expr: Box<Expr<Var>>,
    fields: Vec<Var>, // Chain
  },
  Call {
    function: Var,
    args: Vec<Expr<Var>>,
  },
  If {
    condition: Box<Expr<Var>>,
    t1: Box<Expr<Var>>,
    t2: Box<Expr<Var>>,
  },
  Match(Vec<(Pattern<Var>, Stmt<Var>)>),
}

impl<Var: Debug> Expr<Var> {
  pub fn to_lvalue(self) -> Result<LValue<Var>> {
    match self {
      Expr::Variable(v) => Ok(LValue::Ident(v)),
      Expr::FieldAccess {
        expr: box Expr::Variable(v),
        fields: access,
      } => {
        let mut vec = Vec::with_capacity(access.len() + 1);
        vec.push(v);
        vec.extend(access);
        Ok(LValue::Access(vec))
      }
      other => err(format!("{:?} isn't an l-value", other)),
    }
  }
}
