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
      match self.gstmt() {
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

  fn gstmt(&mut self) -> Result<ast::Gstmt> {
    match self.token()? { 
      Token::TYPE => self.typedef(),
      Token::FUNCTION =>  self.fun_defn(),
      tok => errs(format!("Unknown global token: {:?}", tok))
    }
  }

  // Can be a struct or an enum.
  fn typedef(&mut self) -> Result<ast::Gstmt> {
    let name = self.ident()?;
    self.munch(Token::EQUAL)?;
    let typ = self.typ()?;
    self.munch(Token::SEMICOLON)?;
    self.types.insert(name.clone(), typ.clone());
    Ok(ast::Gstmt::Typedef { typ, name })
  }

  fn arg_list(&mut self) -> Result<ast::Args> { 
    let mut result = Vec::new(); 
    self.munch(Token::LPAREN)?; 
    loop { 
      match self.token()? {
        Token::RPAREN => return Ok(result),
        Token::Ident(ident) => { 
          self.munch(Token::COLON)?; 
          let typ = self.typ()?; 
          result.push((typ,ident))
        },
        tok => return errs(format!("Unknown token {:?} in argument list",tok))
      }
    }
  }

  fn fun_defn(&mut self) -> Result<ast::Gstmt> {
    let name = self.ident()?;
    let args = self.arg_list()?; 
    let typ  = self.typ()?; 
    let body = self.expr()?; 
    Ok(ast::Gstmt::Function { typ, name, args, body })
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


  fn control(&mut self) -> Result<ast::Stmt> {
    use Token::*;
    match self.peek()? {
      RETURN => self.ret_stmt(),
      tok => errs(format!("Invalid token {:?} in control statement.", tok)),
    }
  }

  fn if_stmt(&mut self) -> Result<ast::Expr> {
    errs(String::from("Not yet implemented"))
  }

  fn ret_stmt(&mut self) -> Result<ast::Stmt> {
    self.munch(Token::RETURN)?;
    let expr = self.expr()?;
    Ok(ast::Stmt::Return(box expr))
  }

  // So, the block thing
  fn expr(&mut self) -> Result<ast::Expr> {
    match self.peek()? { 
      Token::LBRACE => { 
        self.skip()?; 
        let mut stmts = Vec::new(); 
        loop { 
          let expr = self.lor_expr()?; 
          stmts.push(ast::Stmt::Expr(expr));
          match self.token()? { 
            Token::SEMICOLON => (),
            Token::RBRACE => return Ok(ast::Expr::Statements(stmts)),
            tok => return errs(format!("Unexpected token {:?} after expression", tok))
          };
        }
      },
      Token::LPAREN => self.paren_expr(),
      tok => return errs(format!("Unexpected token {:?} before expression", tok))
    }
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
