use crate::ast;
use crate::error::{errs, Error, Result};
use crate::lex::{Lexer, Token};
use std::collections::HashMap;
use std::result;

pub struct Parser<'a> {
  lexer: Lexer<'a>,
  // It's possible to intern the types.
  pub types: HashMap<String, ast::Typ>,
  next: Option<Result<Token>>,
}

macro_rules! expr_tier {
    ($name:ident, $next:ident, $($tok:pat = $op:expr),+) => {
        fn $name(&mut self) -> Result<ast::Expr> {
          let mut expr = self.$next()?;
          loop {
            let op = match self.peek() {
              $(Ok(&$tok) => {
                self.skip()?;
                $op
              }),+
              _ => break,
            };
            let rhs = self.$next()?;
            expr = ast::Expr::BinaryOp { op: op, lhs: box expr,rhs: box rhs };
          }
          Ok(expr)
        }
      };
  }

impl<'a> Parser<'a> {
  pub fn parse(&mut self) -> Result<ast::Program> {
    self.program()
  }

  pub fn new(lexer: Lexer<'a>) -> Self {
    Parser {
      lexer,
      types: HashMap::new(),
      next: None,
    }
  }

  pub fn new_types(lexer: Lexer<'a>, types: HashMap<String, ast::Typ>) -> Self {
    Parser {
      lexer,
      types,
      next: None,
    }
  }

  // --------------------------------  HELPERS --------------------------------

  fn token(&mut self) -> Result<Token> {
    if let Some(token) = self.next.take() {
      return token;
    }
    self.lexer.token()
  }

  fn is_type(&mut self) -> Result<bool> {
    use Token::*;
    let val = (*self.peek()?).clone();
    match val {
      Ident(ident) => Ok(self.types.get(&ident).is_some()),
      _ => Ok(false),
    }
  }

  fn peek(&mut self) -> result::Result<&Token, &Error> {
    if self.next.is_none() {
      self.next = Some(self.lexer.token());
    }
    self.next.as_ref().unwrap().as_ref()
  }

  fn skip(&mut self) -> Result<()> {
    self.token()?;
    Ok(())
  }

  fn munch(&mut self, tok: Token) -> Result<()> {
    let ltok = self.token()?;
    if ltok == tok {
      Ok(())
    } else {
      errs(format!("Expected {:?}, got {:?}", tok, ltok))
    }
  }

  // --------------------------------  GRAMMAR --------------------------------
  
  fn program(&mut self) -> Result<ast::Program> {
    let mut program = Vec::new();
    loop {
      match self.gdecl() {
        Ok(stmt) => program.push(stmt),
        Err(Error::EOF) => {
          return Ok(ast::Program(program));
        }
        Err(Error::Message(_e)) => {
          println!("Error: {}", _e);
          return errs(_e);
        }
      }
    }
  }

  fn gdecl(&mut self) -> Result<ast::Gstmt> {
    if self.is_type()? {
      self.fun_defn()
    } else {
      match self.peek()? {
        tok => errs(format!("Could not match {:?} in gstmt", tok)),
      }
    }
  }

  fn typedef(&mut self) -> Result<ast::Gstmt> {
    let typ = self.typ()?;
    let name = self.ident()?;
    self.munch(Token::SEMICOLON)?;
    self.types.insert(name.clone(), typ.clone());
    Ok(ast::Gstmt::Typedef { typ, name })
  }

  fn fun_defn(&mut self) -> Result<ast::Gstmt> {
    let typ = self.typ()?;
    let name = self.ident()?;
    self.munch(Token::LPAREN)?;
    let mut args = Vec::new();
    while self.is_type()? {
      args.push((self.typ()?, self.ident()?));
      match self.peek()? {
        Token::COMMA => self.skip()?,
        _ => break,
      };
    }
    self.munch(Token::RPAREN)?;
    let body = self.block()?;
    Ok(ast::Gstmt::Function {
      typ,
      name,
      args,
      body,
    })
  }

  fn typ(&mut self) -> Result<ast::Typ> {
    match self.token()? {
      Token::Ident(ident) => match self.types.get(&ident) {
        Some(t) => Ok(t.clone()),
        _ => errs(format!("Could not match ident {:?} as type", ident)),
      },
      tok => errs(format!("Could not match {:?} as type", tok)),
    }
  }

  fn ident(&mut self) -> Result<ast::Var> {
    match self.token()? {
      Token::Ident(string) => {
        if !self.types.contains_key(&string) {
          Ok(string)
        } else {
          errs(format!(
            "{:?} is defined as a type, and cannot be an ident.",
            string
          ))
        }
      }
      tok => errs(format!("Could not match {:?} as ident", tok)),
    }
  }

  fn block(&mut self) -> Result<ast::Stmt> {
    let mut stmts = Vec::new();
    self.munch(Token::LBRACE)?;
    loop {
      match self.peek()? {
        // Token::RBRACE => {
        //   self.munch(Token::RBRACE)?;
        //   // Turn : { { stmt } } => { stmt }
        //   if stmts.len() == 1 {
        //     if let ast::Stmt::Block(_) = stmts[0] {
        //       return Ok(stmts.pop().unwrap());
        //     }
        //   };
        //   return Ok(ast::Stmt::Block(stmts));
        // }
        _ => stmts.push(self.stmt()?),
      }
    }
  }

  fn decl(&mut self) -> Result<ast::Stmt> {
    use Token::*;
    let ident = self.ident()?;
    match self.peek()? {
      EQUAL => {
        self.skip()?;
        let expr = self.expr()?;
        Ok(ast::Stmt::Let { name: ident, value: expr})
      }
      tok => errs(format!("Could not match {:?} in decl/defn", tok)),
    }
  }

  fn stmt(&mut self) -> Result<ast::Stmt> {
    use Token::*;
    match self.peek()? {
      IF | LOOP | RETURN => self.control(),
      LBRACE => self.block(),
      _ => {
        let result = self.simp()?;
        self.munch(SEMICOLON)?;
        Ok(result)
      }
    }
  }

  fn simp(&mut self) -> Result<ast::Stmt> {
    use ast::Stmt::*;

    if self.is_type()? {
      return self.decl();
    };

    // There could be an expression here, and we'd never be able to tell
    // the difference without fully parsing it. If it's just a variable, we're fine.
    let maybe_lvalue = self.expr()?;
    Ok(Expr(maybe_lvalue))
  }

  fn simpopt(&mut self) -> Result<ast::Stmt> {
    match self.peek()? {
      _ => self.simp(),
    }
  }

  fn control(&mut self) -> Result<ast::Stmt> {
    use Token::*;
    match self.peek()? {
      // IF => self.if_stmt(),
      RETURN => self.ret_stmt(),
      tok => errs(format!("Invalid token {:?} in control statement.", tok)),
    }
  }

  fn if_stmt(&mut self) -> Result<ast::Expr> {
    errs(String::from("Not yet implemented"))
    // self.munch(Token::IF)?;
    // self.munch(Token::LPAREN)?;
    // let condition = self.expr()?;
    // self.munch(Token::RPAREN)?;
    // let then = box self.block_stmt()?;
    // let Else = box self.block_stmt()?;
    // Ok(ast::Stmt::If { condition, then, Else })
  }



  fn ret_stmt(&mut self) -> Result<ast::Stmt> {
    self.munch(Token::RETURN)?;
    // if let Token::SEMICOLON = self.peek()? {
    //   self.skip()?;
    //   return Ok(ast::Stmt::Return(None));
    // }
    let expr = self.expr()?;
    self.munch(Token::SEMICOLON)?;
    Ok(ast::Stmt::Return(box expr))
  }

  fn expr(&mut self) -> Result<ast::Expr> {
    self.lor_expr()
  }

  fn paren_expr(&mut self) -> Result<ast::Expr> {
    self.munch(Token::LPAREN)?;
    let expr = self.expr()?;
    self.munch(Token::RPAREN)?;
    Ok(expr)
  }

  // --------------------------------  PRECEDENCE  --------------------------------

  // Logical Or
  expr_tier!(lor_expr, land_expr, Token::LOR = ast::BinOp::Or);

  // Logical And
  expr_tier!(land_expr, eq_expr, Token::LAND = ast::BinOp::And);


  // Eq / Neq
  expr_tier!(
    eq_expr,
    comp_expr,
    Token::EQEQ = ast::BinOp::Eql,
    Token::NOTEQ = ast::BinOp::Neq
  );

  expr_tier!(
    comp_expr,
    add_expr,
    Token::LT = ast::BinOp::Lt,
    Token::GT = ast::BinOp::Gt,
    Token::LTE = ast::BinOp::Leq,
    Token::GTE = ast::BinOp::Geq
  );

  expr_tier!(
    add_expr,
    mult_expr,
    Token::MINUS = ast::BinOp::Sub,
    Token::PLUS = ast::BinOp::Add
  );

  expr_tier!(
    mult_expr,
    unary_expr,
    Token::TIMES = ast::BinOp::Mul,
    Token::DIV = ast::BinOp::Div,
    Token::MOD = ast::BinOp::Mod
  );

  fn unary_expr(&mut self) -> Result<ast::Expr> {
    use ast::Expr::*;
    match self.peek()? {
      Token::MINUS | Token::LNOT => {
        let op = self.unop()?;
        let expr = self.primary_expr()?;
        Ok(UnaryOp{ op, rhs: box expr })
      }
      _ => self.primary_expr(),
    }
  }

  // Parse an argument list to a function call
  fn call_list(&mut self) -> Result<Vec<ast::Expr>> {
    self.munch(Token::LPAREN)?;
    let mut args = Vec::new();
    match self.peek()? {
      Token::RPAREN => self.skip()?,
      _ => loop {
        args.push(self.expr()?);
        match self.token()? {
          Token::RPAREN => break,
          Token::COMMA => (),
          tok => return errs(format!("Unexpected token {:?} in function call", tok)),
        };
      },
    };
    Ok(args)
  }

  fn primary_expr(&mut self) -> Result<ast::Expr> {
    use Token::*;
    // We don't want to skip here because of the other
    // stuff that expressions could 'start' with
    match self.peek()? {
      Number(num) => {
        let i = *num;
        self.skip()?;
        Ok(ast::Expr::IntLiteral(i))
      }
      Boolean(val) => {
        let b = *val;
        self.skip()?;
        Ok(ast::Expr::BoolLiteral(b))
      }
      Ident(_) => {
        let s = self.ident()?;
        match self.peek()? {
          Token::LPAREN => Ok(ast::Expr::Call { function: s, args: self.call_list()?}),
          _ => Ok(ast::Expr::Variable(s)),
        }
      }
      LPAREN => self.paren_expr(),
      LNOT => self.unary_expr(),
      tok => errs(format!("Could not match {:?} in primary_expr", tok)),
    }
  }

  fn unop(&mut self) -> Result<ast::UnOp> {
    match self.token()? {
      Token::MINUS => Ok(ast::UnOp::Sub),
      Token::LNOT => Ok(ast::UnOp::Not),
      tok => errs(format!("Could not match token {:?} in unop", tok)),
    }
  }

}
