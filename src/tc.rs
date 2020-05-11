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

/// We provide some convenience implementations of methods on our types.
impl ast::Typ {
  /// Test if self is an integral type, and return the type if so.
  fn integral(&self) -> Result<ast::Typ> {
    use ast::Typ::*;
    match self {
      Int => Ok(Int),
      Float => Ok(Float),
      _ => err("non integral"),
    }
  }

  /// Test if self is a primitive type or not.
  fn primitive(&self) -> bool {
    use ast::Typ::*;
    match self {
      Unit | Int | Float | Bool => true,
      _ => false,
    }
  }

  /// If `self` is a composite type, returns the type of the field on self
  /// named `name`. If not, returns an error. This is valid for structs
  /// and tuples.
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
      _ => err(format!(
        "No field {:?} on non-composite type {:?}",
        name, self
      )),
    }
  }
}

impl ast::BinOp {
  fn synth(&self, lhs: &ast::Typ, rhs: &ast::Typ) -> Result<ast::Typ> {
    use ast::BinOp::*;
    use ast::Typ::*;

    match (self, lhs, rhs) {
      // Arithmetic operations do not automatically convert.
      (Add | Sub | Mul | Div | Mod, &Int, &Int) => Ok(Int),
      (Add | Sub | Mul | Div | Mod, &Float, &Float) => Ok(Float),

      // Comparison operators do not automatically convert.
      (Lt | Gt | Leq | Geq, &Int, &Int) => Ok(Bool),
      (Lt | Gt | Leq | Geq, &Float, &Float) => Ok(Bool),

      // We will have separate, bitwise operators for the appropriate
      // representations of the types.
      (And | Or, &Bool, &Bool) => Ok(Bool),

      // Equal and Not Equal are polymorphic operations.
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

// we use this as a macro to get around some pesky lifetime rules
// that would otherwise be ambiguous - i.e. we want $typ to occasionally
// be a reference with a different lifetime than the one of 'src, but
// it can't be known which one this function will return.
macro_rules! canonical_type {
  ($ctx : ident, $typ : expr) => {{
    let mut t = $typ;
    while let &ast::Typ::Alias(ref alias) = t {
      t = $ctx
        .types
        .get(alias.as_str())
        .expect("undefined type alias");
    }
    t
  }};
}

/// A context for type-checking, which contains a mapping between functions
/// and their signatures, a mapping of type aliases to their resolved types,
/// and a mapping of declared variables currently in scope to their types.
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

  /// Determines if a source type is assignable to another destination type,
  /// i.e. it contains all of the needed information to satisfy the operations
  /// available on the destination type. This corresponds directly to having
  /// a value of t : `src` be assignable to a variable of t : `dest`.
  fn assignable(&mut self, dest: &ast::Typ, src: &ast::Typ) -> Result<()> {
    use ast::Typ::*;

    // We fetch the canonical type, removing type aliases from the equation.
    let (dest, src) = (canonical_type!(self, dest), canonical_type!(self, src));

    // Primitives must be of the same type to be assignable.
    if dest.primitive() {
      if src == dest {
        return Ok(());
      } else {
        return err("primitive mismatch");
      }
    }

    // Currently only structures and tuples are planned to be assignable,
    // Enum variants are named and thus not as loosely interchangable.
    // Function types will also be supported once they are implemented.
    match dest {
      Struct(dest_fields) => {
        if let Struct(src_fields) = src {
          for (df, dt) in dest_fields.iter() {
            let mut found = false;
            for (sf, st) in src_fields.iter() {
              if df == sf {
                self.assignable(dt, st)?;
                found = true;
              }
            }
            if !found {
              return err("field not found");
            }
          }
        }
        return Ok(());
      }
      _ => err(format!("assignability failed from {:?} to {:?}", src, dest)),
    }
  }

  fn tc_expr(&mut self, expr: &'src ast::Expr<ast::Var>) -> Result<ast::Typ> {
    use ast::Expr::*;
    match expr {
      // Todo: add variable scoping
      Block(box expr) => self.tc_expr(expr),

      // Lists of statements have the type of the last statement in the list.
      // If that statement is not an expression, this type will be unit.
      Statements(stmts) => {
        for stmt in stmts[..stmts.len() - 1].iter() {
          self.tc_stmt(stmt)?;
        }
        self.tc_stmt(stmts.last().unwrap())
      }

      // Variables must be declared, or this will not be found.
      Variable(v) => self.typeof_variable(v),

      //
      Call { function, args } => match self.functions.get(function.as_str()) {
        Some(ast::Typ::Function(box ast::Typ::Tuple(arg_typs), box typ)) => {
          if args.len() == arg_typs.len() {
            for (i, arg) in args.iter().enumerate() {
              let t = self.tc_expr(arg)?;
              self.assignable(&arg_typs[i], &t)?;
            }
            Ok(typ.clone())
          } else {
            err("wrong number of arguments to cal")
          }
        }
        _ => err("unexpected function type"),
      },

      //
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

      //
      FieldAccess { expr, fields } => {
        let t = self.tc_expr(expr)?;
        self.typeof_access(&t, fields)
      }

      //
      WithExpression { expr, fields } => {
        let t = self.tc_expr(expr)?;
        let t = canonical_type!(self, &t);
        for (var, field_expr) in fields.iter() {
          let assign_type = self.tc_expr(field_expr)?;
          let field_type = &t.field(var)?;
          let field_type = canonical_type!(self, field_type);
          if field_type != &assign_type {
            return err("mismatch in with-expression");
          }
        }
        Ok(t.clone())
      }
      IntLiteral(_) => Ok(ast::Typ::Int),
      BoolLiteral(_) => Ok(ast::Typ::Bool),
      FloatLiteral(_) => Ok(ast::Typ::Float),
      StructLiteral { name, fields } => {
        let t = match self.types.get(name.as_str()) {
          Some(&t) => t,
          None => return err("unknown struct type"),
        };
        let t = canonical_type!(self, t);
        for (var, field_expr) in fields.iter() {
          let assign_type = self.tc_expr(field_expr)?;
          let field_type = t.field(var)?;
          if canonical_type!(self, &assign_type) != canonical_type!(self, &field_type) {
            return err("mismatch in struct literal");
          }
        }
        Ok(t.clone())
      }
      BinaryOp {
        op,
        box lhs,
        box rhs,
      } => {
        let t1 = self.tc_expr(&lhs)?;
        let t2 = self.tc_expr(&rhs)?;
        op.synth(&t1, &t2)
      }
      UnaryOp { op, box rhs } => {
        use ast::Typ::*;
        use ast::UnOp::*;
        let t1 = self.tc_expr(&rhs)?;
        match (op, &t1) {
          (Not, Bool) => Ok(t1),
          (Sub, Int | Float) => Ok(t1),
          _ => err("type mismatch unop"),
        }
      }
      TupleLiteral(exprs) => {
        let mut types = vec![ast::Typ::Unit; exprs.len()];
        for (i, expr) in exprs.iter().enumerate() {
          types[i] = self.tc_expr(expr)?;
        }
        Ok(ast::Typ::Tuple(types))
      }
      EnumLiteral { name, args } => {
        for (_, typ) in self.types.iter() {
          if let ast::Typ::Enum(fields) = typ {
            for (field, field_typ) in fields.iter() {
              if field == name {
                if args.len() == 1 {
                  let t = self.tc_expr(&args[0])?;
                  if field_typ == &t {
                    return Ok(t);
                  } else {
                    return err("single-field enum mismatch");
                  }
                } else {
                  let mut types = vec![ast::Typ::Unit; args.len()];
                  for (i, expr) in args.iter().enumerate() {
                    types[i] = self.tc_expr(expr)?;
                  }
                  let result = ast::Typ::Tuple(types);
                  if field_typ == &result {
                    return Ok(result);
                  } else {
                    return err("multi-field enum mismatch");
                  }
                }
              }
            }
          }
        }
        err("matching enum not found")
      }
      Match(_) | AsExpression { .. } => unimplemented!(),
    }
  }

  fn typeof_variable(&self, var: &'src ast::Var) -> Result<ast::Typ> {
    match self.variables.get(var.as_str()) {
      Some(e) => Ok(e.clone()),
      None => err("undefined variable"),
    }
  }

  fn typeof_access(&self, base: &ast::Typ, access: &[ast::Var]) -> Result<ast::Typ> {
    let mut base_type = canonical_type!(self, base); // Not 'Alias(_)'
    for access_field in access.iter() {
      match base_type {
        ast::Typ::Struct(ref typed_fields) => {
          for (field, typ) in typed_fields.iter() {
            if field == access_field {
              base_type = canonical_type!(self, typ);
              break;
            }
          }
        }
        ast::Typ::Tuple(ref types) => {
          // Attempt to parse the "field" as a tuple index
          match access_field.parse::<usize>() {
            Ok(i) => {
              if i < types.len() {
                let t = &types[i];
                base_type = canonical_type!(self, t);
                break;
              }
            }
            Err(_) => return err("not a tuple field"),
          }
        }
        _ => return err("cant index non-composite type"),
      }
    }
    Ok(base_type.clone())
  }

  fn typeof_lvalue(&self, lvalue: &'src ast::LValue<ast::Var>) -> Result<ast::Typ> {
    use ast::*;
    match lvalue {
      LValue::Ident(v) => self.typeof_variable(v),
      LValue::Access(access) => {
        if access.len() < 2 {
          return err("access isn't an access");
        }
        let base_type = self.typeof_variable(&access[0])?;
        self.typeof_access(&base_type, &access[1..])
      }
    }
  }

  fn tc_stmt(&mut self, stmt: &'src ast::Stmt<ast::Var>) -> Result<ast::Typ> {
    use ast::Stmt::*;
    match stmt {
      BREAK => Ok(ast::Typ::Unit),
      //
      Expr(e) => self.tc_expr(e),

      //
      Loop(e) | Return(e) => {
        self.tc_expr(e)?;
        Ok(ast::Typ::Unit)
      }
      // Declare and define a value.
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

  fn tc_function(&mut self, return_type: &ast::Typ, body: &'src ast::Expr<ast::Var>) -> Result<()> {
    match self.tc_expr(body) {
      Ok(typ) => {
        let return_type = canonical_type!(self, return_type);
        if &typ == return_type {
          Ok(())
        } else {
          err(format!(
            "type mismatch: expected {:?}, got {:?}",
            return_type, typ
          ))
        }
      }
      Err(e) => Err(e),
    }
  }
}

pub fn typecheck<'a>(program: &'a ast::Program<ast::Var>) -> Result<()> {
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
