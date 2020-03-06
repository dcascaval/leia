// Leia Compiler
//! Typechecking & Static Analysis
//
// Todo: Miscellaneous allocation / perf improvements
//
//   String internment
//      Add a small mapping to get a small integer value for idents. This has
//      the advantage of not incurring allocation, and also being Copy, thus
//      avoiding all of the fun we're having with 'src lifetimes caused by
//      directly hashing and storing identifiers.
//
//   Type alias resolution / internment.
//       Basically, we want to avoid ever having the type of variables be an
//       aliased type, so we perform the look-up of the alias immediately and
//       intern the resolved type, thus avoiding many unnecessary allocations.

use crate::ast;
use crate::error::*;
use std::collections::HashMap;

impl ast::Typ {
  fn integral(&self) -> Result<ast::Typ> {
    match self {
      ast::Typ::Int => Ok(ast::Typ::Int),
      ast::Typ::Float => Ok(ast::Typ::Float),
      _ => err("non integral"),
    }
  }

  fn field(&self, name: &ast::Var) -> Result<ast::Typ> {
    match self {
      ast::Typ::Struct(ref typed_fields) => {
        for (field, typ) in typed_fields.iter() {
          if field == name {
            return Ok(typ.clone());
          }
        }
        err("no field name on type")
      }
      // Attempt to parse the "field" as a tuple index
      ast::Typ::Tuple(ref types) => match name.parse::<usize>() {
        Ok(i) if i < types.len() => Ok(types[i].clone()),
        _ => err("name not a tuple field"),
      },
      _ => err("no fields on non-composite type"),
    }
  }
}

impl ast::BinOp {
  fn synth(&self, lhs: &ast::Typ, rhs: &ast::Typ) -> Result<ast::Typ> {
    use ast::BinOp::*;
    use ast::Typ::*;

    // Todo: not quite right for mod
    match (self, lhs, rhs) {
      (Add | Sub | Mul | Div | Mod, &Int, &Int) => Ok(Int),
      (Add | Sub | Mul | Div | Mod, &Float, &Float) => Ok(Float),
      (Lt | Gt | Leq | Geq, &Int, &Int) => Ok(Bool),
      (Lt | Gt | Leq | Geq, &Float, &Float) => Ok(Bool),
      (And | Or, &Bool, &Bool) => Ok(Bool),
      (Eql | Neq, t1, t2) => {
        if t1 == t2 {
          Ok(t1.clone())
        } else {
          err("non-poly-match-eq")
        }
      }
      _ => err("non-match binop"),
    }
  }
}

struct Context<'src, 'fun> {
  functions: &'fun HashMap<&'src str, ast::Typ>,
  types: &'fun HashMap<&'src str, &'src ast::Typ>,
  variables: &'fun mut HashMap<&'src str, ast::Typ>,
}

impl<'src, 'fun> Context<'src, 'fun> {
  fn new(
    functions: &'fun HashMap<&'src str, ast::Typ>,
    types: &'fun HashMap<&'src str, &'src ast::Typ>,
    variables: &'fun mut HashMap<&'src str, ast::Typ>,
  ) -> Self {
    Context {
      functions,
      types,
      variables,
    }
  }

  fn define(&mut self, var: &'src ast::Var, typ: ast::Typ) {
    self.variables.insert(var, typ);
  }

  fn tc_expr(&mut self, expr: &'src ast::Expr) -> Result<ast::Typ> {
    use ast::Expr::*;
    match expr {
      // Todo: add variable scoping
      Block(box expr) => self.tc_expr(expr),
      Statements(stmts) => {
        for stmt in stmts[..stmts.len() - 1].iter() {
          self.tc_stmt(stmt)?;
        }
        self.tc_stmt(stmts.last().unwrap())
      }
      Variable(v) => self.typeof_variable(v),
      Call { function, args } => match self.functions.get(function.as_str()) {
        Some(ast::Typ::Function(box typ, box ast::Typ::Tuple(arg_typs))) => {
          if args.len() == arg_typs.len() {
            for (i, arg) in args.iter().enumerate() {
              let t = self.tc_expr(arg)?;
              if t != arg_typs[i] {
                return err("type mismatch in function args");
              }
            }
            Ok(typ.clone())
          } else {
            err("wrong number of arguments to cal")
          }
        }
        _ => err("unexpected function type"),
      },
      If { condition, t1, t2 } => {
        if let ast::Typ::Bool = self.tc_expr(condition)? {
          let typ1 = self.tc_expr(t1)?;
          let typ2 = self.tc_expr(t2)?;
          if typ1 == typ2 {
            return Ok(typ1);
          } else {
            return err("type mismatch in condition");
          }
        } else {
          return err("non-boolean conditional header");
        }
      }
      FieldAccess { expr, fields } => {
        let t = self.tc_expr(expr)?;
        self.typeof_access(t, fields)
      }
      WithExpression { expr, fields } => {
        let t = self.tc_expr(expr)?;
        for (var, field_expr) in fields.iter() {
          let e = self.tc_expr(field_expr)?;
          let v = self.canonical_type(&t.field(var)?)?;
          if v != e {
            return err("mismatch in with-expression");
          }
        }
        Ok(t)
      }
      IntLiteral(_) => Ok(ast::Typ::Int),
      BoolLiteral(_) => Ok(ast::Typ::Bool),
      FloatLiteral(_) => Ok(ast::Typ::Float),
      StructLiteral { name, fields } => {
        let t = match self.types.get(name.as_str()) {
          Some(&t) => t,
          None => return err("unknown struct type"),
        };
        for (var, field_expr) in fields.iter() {
          let e = self.tc_expr(field_expr)?;
          let v = self.canonical_type(&t.field(var)?)?;
          if v != e {
            return err("mismatch in struct literal");
          }
        }
        Ok(t.clone())
      }
      BinaryOp { .. } => unimplemented!(),
      UnaryOp { .. } => unimplemented!(),
      EnumLiteral { .. } => unimplemented!(),
      TupleLiteral { .. } => unimplemented!(),
      Match(_) | AsExpression { .. } => unimplemented!(),
    }
  }

  fn typeof_variable(&self, var: &'src ast::Var) -> Result<ast::Typ> {
    match self.variables.get(var.as_str()) {
      Some(e) => Ok(e.clone()),
      None => err("undefined variable"),
    }
  }

  fn canonical_type(&self, typ: &ast::Typ) -> Result<ast::Typ> {
    match typ {
      ast::Typ::Alias(ref alias) => match self.types.get(alias.as_str()) {
        Some(&v) => Ok(v.clone()),
        None => err("undefined type alias"),
      },
      t => Ok(t.clone()),
    }
  }

  fn typeof_access(&self, base: ast::Typ, access: &[ast::Var]) -> Result<ast::Typ> {
    let mut base_type = self.canonical_type(&base)?; // Not 'Alias(_)'
    for access_field in access.iter() {
      match base_type {
        ast::Typ::Struct(ref typed_fields) => {
          for (field, typ) in typed_fields.iter() {
            if field == access_field {
              base_type = self.canonical_type(typ)?;
              break;
            }
          }
        }
        ast::Typ::Tuple(ref types) => {
          // Attempt to parse the "field" as a tuple index
          match access_field.parse::<usize>() {
            Ok(i) => {
              if i < types.len() {
                base_type = self.canonical_type(&types[i])?;
                break;
              }
            }
            Err(_) => return err("not a tuple field"),
          }
        }
        _ => return err("cant index non-composite type"),
      }
    }
    Ok(base_type)
  }

  fn typeof_lvalue(&self, lvalue: &'src ast::LValue) -> Result<ast::Typ> {
    use ast::*;
    match lvalue {
      LValue::Ident(v) => self.typeof_variable(v),
      LValue::Access(access) => {
        if access.len() < 2 {
          return err("access isn't an access");
        }
        let base_type = self.typeof_variable(&access[0])?;
        self.typeof_access(base_type, &access[1..])
      }
    }
  }

  fn tc_stmt(&mut self, stmt: &'src ast::Stmt) -> Result<ast::Typ> {
    use ast::Stmt::*;
    match stmt {
      BREAK => Ok(ast::Typ::Unit),
      Expr(e) => self.tc_expr(e),
      Loop(e) | Return(e) => {
        self.tc_expr(e)?;
        Ok(ast::Typ::Unit)
      }
      Let { name, value } => {
        let typ = self.tc_expr(value)?;
        self.define(name, typ);
        Ok(ast::Typ::Unit)
      }
      // Get the type of the lvalue
      Assign { target, value } => {
        let t1 = self.typeof_lvalue(target)?;
        let t2 = self.tc_expr(value)?;
        if t1 == t2 {
          Ok(ast::Typ::Unit)
        } else {
          err("type mismatch in assign")
        }
      }
    }
  }

  fn tc_function(&mut self, return_type: &ast::Typ, body: &'src ast::Expr) -> Result<()> {
    match self.tc_expr(body) {
      Ok(typ) => {
        if &typ == return_type {
          Ok(())
        } else {
          err("type mismatch")
        }
      }
      Err(e) => Err(e),
    }
  }
}

pub fn typecheck<'a>(program: &'a ast::Program) -> Result<()> {
  let mut known_types: HashMap<&'a str, &ast::Typ> = HashMap::new();
  let mut known_functions: HashMap<&'a str, ast::Typ> = HashMap::new();
  for gstmt in program.0.iter() {
    match gstmt {
      ast::Gstmt::Typedef { name, typ } => {
        known_types.insert(name, typ);
      }
      ast::Gstmt::Function {
        name,
        args,
        typ,
        body: _,
      } => {
        let arg_types = ast::Typ::Tuple(args.iter().map(|a| a.0.clone()).collect());
        known_functions.insert(name, ast::Typ::Function(box arg_types, box typ.clone()));
      }
    }
  }
  for gstmt in program.0.iter() {
    if let ast::Gstmt::Function {
      name: _,
      args,
      typ,
      body,
    } = gstmt
    {
      let mut variables: HashMap<&'a str, ast::Typ> = HashMap::new();
      for arg in args {
        variables.insert(&arg.1, arg.0.clone());
      }
      let mut ctx = Context::new(&known_functions, &known_types, &mut variables);
      ctx.tc_function(typ, body)?;
    }
  }
  Ok(())
}
