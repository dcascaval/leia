// Leia Compiler
//! Typechecking & Static Analysis
//

use crate::ast;
use crate::error::*;
use std::collections::HashMap;

struct Context<'src,'fun> {
  functions : &'fun HashMap<&'src str,ast::Typ>, 
  types : &'fun HashMap<&'src str,&'src ast::Typ>,
  variables : &'fun HashMap<&'src str,&'src ast::Typ>
}

impl<'src,'fun> Context<'src,'fun> { 
  fn new(functions : &'fun HashMap<&'src str,ast::Typ>, 
         types     : &'fun HashMap<&'src str,&'src ast::Typ>,
         variables : &'fun HashMap<&'src str,&'src ast::Typ>) -> Self { 
    Context { functions, types, variables }
  }

  fn tc_expr(&mut self, _expr: &ast::Expr) -> Result<ast::Typ> { 
    unimplemented!()
  }
  
  fn tc_stmt(&mut self, _stmt: &ast::Stmt) -> Result<()> { 
    unimplemented!()
  }
  
  fn tc_function(&mut self, return_type : &ast::Typ, body : &ast::Expr) -> Result<()> {
    match self.tc_expr(body) { 
      Ok(typ) => { 
        if &typ == return_type { Ok(()) } 
        else { 
          err("type mismatch" )
      }},
      Err(e) => Err(e)
    }
  }
}

pub fn typecheck<'a>(program : &'a ast::Program) -> Result<()> { 
  let mut known_types : HashMap<&'a str,&ast::Typ> = HashMap::new();
  let mut known_functions : HashMap<&'a str,ast::Typ> = HashMap::new();
  for gstmt in program.0.iter() { 
    match gstmt {
      ast::Gstmt::Typedef{ name, typ } => {
        known_types.insert(name,typ);
      },
      ast::Gstmt::Function{ name, args, typ, body: _ } => {
        let arg_types = ast::Typ::Tuple(args.iter().map(|a| a.0.clone()).collect());
        known_functions.insert(name, ast::Typ::Function(box arg_types,box typ.clone()));
      }
    }
  }
  for gstmt in program.0.iter() { 
    if let ast::Gstmt::Function{ name: _ , args, typ, body } = gstmt { 
      let mut variables : HashMap<&'a str,&ast::Typ> = HashMap::new();
      for arg in args { 
        variables.insert(&arg.1, &arg.0);
      }
      let mut ctx = Context::new(&known_functions, &known_types, &variables);
      ctx.tc_function(typ,body)?;
    }
  }
  Ok(())
}