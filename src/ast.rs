use std::fmt::Debug;

pub type Var = String;

// Todo: check eq impl
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Typ {
  Int,
  Bool,
  Float,
  Alias (Var),
  Composite (Box<Typ>,Var), 
  Struct (Vec<(String,Typ)>),
  Enum (Vec<(String,Typ)>),
  Tuple (Vec<Typ>),
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
pub struct Program(pub Vec<Gstmt>);

pub type Args = Vec<(Typ, Var)>;

#[derive(Debug)]
pub enum Gstmt {
  Typedef {
    name: Var, 
    typ: Typ,
  },
  Function {
    typ: Typ,
    name: Var,
    args: Args,
    body: Expr,
  },
}

#[derive(Debug,Clone)]
pub enum LValue { 
  Ident(Var), 
  Access(Vec<Var>),
}

// Todo: make as structs?
#[derive(Debug,Clone)]
pub enum Stmt {
  Let { 
    name: Var,
    value: Expr
  }, 
  Assign { 
    target: LValue, 
    value: Expr 
  },
  Expr(Expr),
  Loop(Expr),
  Return(Expr),
  BREAK
}

#[derive(Debug,Clone)]
pub enum Pattern {
  IntLiteral(i64),
  FloatLiteral(f64),
  BooleanLiteral(bool), 
  IntRange { 
    min: i64,
    max: i64 
  }, 
  FloatRange { 
    min: f64, 
    max: f64
  },
  Struct(Vec<(Var,Pattern)>), 
  Enum { 
    name: Var, 
    args: Vec<Pattern> 
  },
  Variable(Var),
}

#[derive(Debug,Clone)]
pub enum Expr {
  Block(Box<Expr>),
  Statements(Vec<Stmt>),
  IntLiteral(i64),
  BoolLiteral(bool), 
  FloatLiteral(f64),
  StructLiteral { 
    name: Var,
    fields: Vec<(Var,Expr)>,
  },
  EnumLiteral { // Like a function call!
    name: Var, 
    args: Vec<Expr>
  },
  TupleLiteral(Vec<Expr>),
  AsExpression { 
    expr: Box<Expr>,
    target: Typ
  },
  WithExpression { 
    expr: Box<Expr>, 
    fields: Vec<(Var,Expr)>,
  },
  BinaryOp { 
    op: BinOp,
    lhs: Box<Expr>,
    rhs: Box<Expr>
  },
  UnaryOp { 
    op: UnOp,
    rhs: Box<Expr>
  },
  Variable(Var),
  FieldAccess { 
    expr: Box<Expr>,
    fields: Vec<Var> // Chain
  }, 
  Call { 
    function: Var,
    args: Vec<Expr>
  },
  If { 
    condition: Box<Expr>,
    t1 : Box<Expr>, 
    t2 : Box<Expr>
  },
  Match(Vec<(Pattern,Stmt)>),
}




