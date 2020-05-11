use crate::ast;
use std::collections::HashMap;

pub struct Interner {
  arena: Vec<ast::Var>,
  mapping: HashMap<ast::Var, u32>,
}

impl Interner {
  // Can we improve on this one by removing some allocations?
  // There is only one right now, though, so perhaps not.
  // For example, some code storing everything in one large string:
  // https://gist.github.com/CAD97/036c700fad1b4b159421eca089783122
  pub fn new() -> Self {
    Self {
      arena: Vec::new(),
      mapping: HashMap::new(),
    }
  }

  pub fn lookup(&self, index: &u32) -> &ast::Var {
    &self.arena[*index as usize]
  }

  pub fn intern(&mut self, var: ast::Var) -> u32 {
    match self.mapping.get(&var) {
      Some(v) => *v,
      None => {
        let result = self.arena.len() as u32;
        self.mapping.insert(var.clone(), result);
        self.arena.push(var);
        result
      }
    }
  }
}

fn convert_fields(
  i: &mut Interner,
  fields: Vec<(String, ast::Expr<String>)>,
) -> Vec<(u32, ast::Expr<u32>)> {
  fields
    .into_iter()
    .map(|(f, e)| (i.intern(f), convert_expr(i, e)))
    .collect()
}

// A large percentage of this is just forwarding.
// We only do anything actually relevant on:
// Tuple, As, Struct, With, Call, and Variable
fn convert_expr(i: &mut Interner, expr: ast::Expr<String>) -> ast::Expr<u32> {
  use ast::Expr::*;
  match expr {
    Block(box e) => Block(box convert_expr(i, e)),
    Statements(s) => Statements(s.into_iter().map(|s| convert_stmt(i, s)).collect()),
    IntLiteral(i) => IntLiteral(i),
    BoolLiteral(b) => BoolLiteral(b),
    FloatLiteral(f) => FloatLiteral(f),
    StructLiteral { name, fields } => ast::Expr::StructLiteral {
      name: i.intern(name),
      fields: convert_fields(i, fields),
    },
    TupleLiteral(fields) => TupleLiteral(fields.into_iter().map(|e| convert_expr(i, e)).collect()),
    AsExpression { box expr, target } => AsExpression {
      expr: box convert_expr(i, expr),
      target,
    },
    WithExpression { box expr, fields } => WithExpression {
      expr: box convert_expr(i, expr),
      fields: convert_fields(i, fields),
    },
    BinaryOp {
      op,
      box lhs,
      box rhs,
    } => BinaryOp {
      op,
      lhs: box convert_expr(i, lhs),
      rhs: box convert_expr(i, rhs),
    },
    UnaryOp { op, box rhs } => UnaryOp {
      op,
      rhs: box convert_expr(i, rhs),
    },
    Variable(v) => Variable(i.intern(v)),
    FieldAccess { box expr, fields } => FieldAccess {
      expr: box convert_expr(i, expr),
      fields: fields.into_iter().map(|f| i.intern(f)).collect(),
    },
    Call { function, args } => Call {
      function: i.intern(function),
      args: args.into_iter().map(|e| convert_expr(i, e)).collect(),
    },
    If {
      box condition,
      box t1,
      box t2,
    } => If {
      condition: box convert_expr(i, condition),
      t1: box convert_expr(i, t1),
      t2: box convert_expr(i, t2),
    },
    Match(_) => todo!(),
    EnumLiteral { .. } => todo!(),
  }
}

fn convert_lval(i: &mut Interner, lval: ast::LValue<String>) -> ast::LValue<u32> {
  match lval {
    ast::LValue::Ident(n) => ast::LValue::Ident(i.intern(n)),
    ast::LValue::Access(vars) => {
      ast::LValue::Access(vars.into_iter().map(|f| i.intern(f)).collect())
    }
  }
}

fn convert_stmt(i: &mut Interner, stmt: ast::Stmt<String>) -> ast::Stmt<u32> {
  use ast::Stmt::*;
  match stmt {
    Let { name, value } => Let {
      name: i.intern(name),
      value: convert_expr(i, value),
    },
    Assign { target, value } => Assign {
      target: convert_lval(i, target),
      value: convert_expr(i, value),
    },
    Expr(expr) => Expr(convert_expr(i, expr)),
    Loop(expr) => Loop(convert_expr(i, expr)),
    Return(e) => Return(convert_expr(i, e)),
    BREAK => BREAK,
  }
}

// It's possible that the parser could intern strings for us directly.
// We don't want to do this mainly so that it's easier to display errors in the type-checking
// stage, and so that pretty-printing and whatnot works. That said, I'm sure we can be smart
// with the lookup, so it's something to consider later if performance in that area
// turns out to be an issue. My hypothesis is that it won't be.
pub fn convert(program: ast::Program<String>) -> (Interner, ast::Program<u32>) {
  let mut intern = Interner::new();
  let mut new_program = Vec::with_capacity(program.0.len());
  for gstmt in program.0.into_iter() {
    if let ast::Gstmt::Function {
      args, body, name, ..
    } = gstmt
    {
      let args = args
        .into_iter()
        .map(|(t, s)| (t, intern.intern(s)))
        .collect();
      let body = convert_expr(&mut intern, body);
      let name = intern.intern(name);
      new_program.push(ast::Gstmt::Function {
        args,
        body,
        name,
        typ: ast::Typ::Unit,
      })
    }
  }
  (intern, ast::Program(new_program))
}
