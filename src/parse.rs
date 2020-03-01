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
    dbg!(self.lexer.token())
  }

  fn peek(&mut self) -> result::Result<&Token, &Error> {
    if self.next.is_none() {
      self.next = Some(dbg!(self.lexer.token()));
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
      Token::FUNCTION => self.fun_defn(),
      tok => errs(format!("Unknown global token: {:?}", tok))
    }
  }

  fn ret_stmt(&mut self) -> Result<ast::Stmt> {
    self.munch(Token::RETURN)?;
    Ok(ast::Stmt::Return(self.expr()?))
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
    self.munch(Token::LPAREN)?; 
    let mut result = Vec::new(); 
    if let Token::RPAREN = self.peek()?  {
      self.skip()?; 
      return Ok(result);
    }
    loop { 
      match self.token()? {
        Token::Ident(ident) => { 
          self.munch(Token::COLON)?; 
          let typ = self.typ()?; 
          result.push((typ,ident));
          match self.peek()? {
            Token::RPAREN => { self.skip()?; return Ok(result) },
            Token::COMMA => self.skip()?,
            tok => return errs(format!("Unknown token {:?} in argument list",tok))
          }
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

  fn field(&mut self) -> Result<(String,ast::Typ)> { 
    let id = self.ident()?; 
    self.munch(Token::COLON)?; 
    let typ = self.typ()?;
    Ok((id,typ))
  }

  
  fn literal_fields(&mut self) -> Result<Vec<(String,ast::Expr)>> { 
    let mut fields = Vec::new();
    if let Token::RBRACE = self.peek()? { 
      return Ok(fields)
    };
    loop { 
      match self.peek()? { 
        Token::Ident(_) => {
          let id = self.ident()?; 
          self.munch(Token::COLON)?; 
          let expr = self.expr()?; 
          fields.push((id,expr));
          match self.peek()? { 
            Token::RBRACE => { self.skip()?; return Ok(fields) },
            Token::SEMICOLON => self.skip()?,
            tok => return errs(format!("Expected closing brace or semicolon, got {:?}", tok))
          }
        },
        tok => return errs(format!("Expected name of struct field in literal, got {:?}", tok))
      }
    }
  }

  fn typ(&mut self) -> Result<ast::Typ> {
    match self.peek()? { 
      Token::LPAREN => { // Tuple
        self.skip()?; 
        let mut typs = Vec::new();
        if let Token::RPAREN = self.peek()? {
          self.skip()?; 
          return Ok(ast::Typ::Tuple(typs)) 
        }
        loop { 
          typs.push(self.typ()?);
          match self.token()? { 
            Token::COMMA => (),
            Token::RPAREN => return Ok(ast::Typ::Tuple(typs)),
            tok => return errs(format!("Expected comma or closing paren, got {:?}",tok)),
          }
        }
      },
      Token::LBRACE => { // Anonymous struct
        self.skip()?; 
        let mut fields = Vec::new(); 
        if let Token::RBRACE = self.peek()? { 
          self.skip()?; 
          return Ok(ast::Typ::Struct(fields))
        }
        loop { 
          fields.push(self.field()?); 
          match self.token()? { 
            Token::COMMA => (), 
            Token::RBRACE => return Ok(ast::Typ::Struct(fields)),
            tok => return errs(format!("Expected comma or closing brace, got {:?}",tok))
          }
        }
      },
      // Composite types work as follows: 
      // First, we parse a 'type'. Then, all of the following
      // type keywords after it are modifiers. There are not
      // currently any pointer types.
      Token::Ident(_) => {
        let mut id = self.ident()?; 
        match id.as_str() { 
          "int" => return Ok(ast::Typ::Int),
          "bool" => return Ok(ast::Typ::Bool), 
          "float" => return Ok(ast::Typ::Float),
          _ => (),
        };
        let mut next = match self.peek()? { 
          Token::COLON => { // enum field
            let mut fields = Vec::new();
            loop {
              self.munch(Token::COLON)?; 
              let t = self.typ()?;
              fields.push((id,t));
              match self.peek()? { 
                Token::PIPE => { self.skip()?; id = self.ident()?;  }
                _ => { break ast::Typ::Enum(fields) } 
              }
            }
          },
          _ => ast::Typ::Alias(id)
        }; 
        loop { 
          match self.peek()? { 
            Token::Ident(_) => { 
              let id = self.ident()?; 
              next = ast::Typ::Composite(box next,id) 
            }
            _ => return Ok(next)
          }
        }
      }, 
      tok => return errs(format!("Expected opening paren, brace, or identifier, got {:?}",tok))
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

  fn expr(&mut self) -> Result<ast::Expr> {
    match self.peek()? { 
      Token::LBRACE => self.block_expr(),
      Token::LPAREN => self.paren_expr(),
      Token::Number(_) | Token::Ident(_) => self.lor_expr(),
      tok => return errs(format!("Unexpected token {:?} before expression", tok))
    }
  }

  fn stmt(&mut self) -> Result<ast::Stmt> { 
    match self.peek()? { 
      Token::BREAK => Ok(ast::Stmt::BREAK),
      Token::LET => self.let_stmt(),
      Token::RETURN => self.ret_stmt(), 
      Token::LOOP => self.loop_stmt(), // These should be exprs, but aren't yet
      _ => Ok(ast::Stmt::Expr(self.expr()?)) 
    }
  }

  fn loop_stmt(&mut self) -> Result<ast::Stmt> { 
    self.munch(Token::LOOP)?; 
    Ok(ast::Stmt::Loop(self.expr()?))
  }

  // Todo: patterns
  // todo: mut
  fn let_stmt(&mut self) -> Result<ast::Stmt> { 
    self.munch(Token::LET)?; 
    let name = self.ident()?;  
    self.munch(Token::EQUAL)?; 
    let value = self.expr()?; 
    Ok(ast::Stmt::Let{ name, value})
  }

  fn block_expr(&mut self) -> Result<ast::Expr> { 
    self.munch(Token::LBRACE)?; 
    let mut stmts = Vec::new(); 
    loop { 
      let expr = self.stmt()?; 
      stmts.push(expr);
      match self.token()? { 
        Token::SEMICOLON => (),
        Token::RBRACE => return Ok(ast::Expr::Statements(stmts)),
        tok => return errs(format!("Unexpected token {:?} after expression", tok))
      };
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
    as_expr,
    Token::TIMES = ast::BinOp::Mul,
    Token::DIV = ast::BinOp::Div,
    Token::MOD = ast::BinOp::Mod
  );

  // Binds tighter than any other expression except a unary one -- 
  // because it basically _is_ a right-unary one, with the additional
  // argument of the type on the right hand side. (We might eventually want to make
  // it bind even tighter than that.)
  fn as_expr(&mut self) -> Result<ast::Expr> { 
    let expr = self.unary_expr()?;
    match self.peek()? { 
      Token::AS => { 
        self.skip()?;
        let target = self.typ()?; 
        Ok(ast::Expr::AsExpression { expr: box expr, target })
      }
      _ => self.unary_expr(),
    }
  }

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

  fn cond_expr(&mut self) -> Result<ast::Expr> { 
    self.munch(Token::IF)?; 
    let condition = box self.expr()?; 
    self.munch(Token::THEN)?; 
    let t1 = box self.expr()?; 
    self.munch(Token::ELSE)?; 
    let t2 = box self.expr()?;
    Ok(ast::Expr::If { condition, t1, t2 })
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
      &Number(n) => {
        self.skip()?;
        Ok(ast::Expr::IntLiteral(n))
      }
      &Boolean(b) => {
        self.skip()?;
        Ok(ast::Expr::BoolLiteral(b))
      }
      Ident(_) => {
        let s = self.ident()?;
        match self.peek()? {
          Token::LPAREN => Ok(ast::Expr::Call { 
            function: s, 
            args: self.call_list()?
          }),
          Token::LBRACE => Ok(ast::Expr::StructLiteral { 
            name: s,
            fields: self.literal_fields()? 
          }), 
          _ => Ok(ast::Expr::Variable(s)),
        }
      }
      IF => self.cond_expr(),
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

