// Leia Compiler
//! Parser
/// Hand-rolled, recursive-descent approach using a variant of top-down
/// operator precedence. This should directly encode the grammar in `docs/grammar.txt`
// Todo:
// Add positional information in the form of source code spans.
// Here too, we should add error reasonable error messages, as well as attempted
// recovery (see `src/lex.rs`). We'll probably implement this as some sort of
// error context stack. It's as yet unclear how we'll get to the end of the displayed
// expression: should we go to the next line? Should we find the position of the next
// delimiting symbol in source (up to a limit) and go there?
// How should we display the error w/r/t the token at which it arose?

// We'll want to fuzz the parser to make sure it can't crash on any valid programs,
// and that it doesn't accidentally drop any parsed expressions because it hit
// some error.
use crate::ast;
use crate::error::{err, Error, Result};
use crate::lex::{Lexer, Token};
use std::result;

pub struct Parser<'a> {
  lexer: Lexer<'a>,
  next: Option<Result<Token>>,
}

macro_rules! parse_error {
  ($lexer:ident, $($arg:expr),+) => {{
    let msg = format!($($arg),+);
    Parser::error($lexer,msg)
  }};
}

impl<'a> Parser<'a> {
  pub fn parse(&mut self) -> Result<ast::Program<ast::Var>> {
    self.program()
  }

  pub fn new(lexer: Lexer<'a>) -> Self {
    Parser { lexer, next: None }
  }

  // --------------------------------  HELPERS --------------------------------

  fn token(&mut self) -> Result<Token> {
    if let Some(token) = self.next.take() {
      return token;
    }
    let tok = self.lexer.token();
    // println!("{:?}",tok);
    tok
  }

  fn peek(&mut self) -> result::Result<&Token, &Error> {
    if self.next.is_none() {
      let tok = self.lexer.token();
      // println!("{:?}",tok);
      self.next = Some(tok);
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
      parse_error!(self, "Expected {:?}, got {:?}", tok, ltok)
    }
  }

  // --------------------------------  GRAMMAR --------------------------------

  fn program(&mut self) -> Result<ast::Program<ast::Var>> {
    let mut program = Vec::new();
    loop {
      match self.gstmt() {
        Ok(stmt) => program.push(stmt),
        Err(Error::EOF) => {
          return Ok(ast::Program(program));
        }
        Err(Error::Message(e)) => {
          return err(e);
        }
      }
    }
  }

  fn gstmt(&mut self) -> Result<ast::Gstmt<ast::Var>> {
    match self.token()? {
      Token::TYPE => self.typedef(),
      Token::FUNCTION => self.fun_defn(),
      tok => parse_error!(self, "Unknown global token: {:?}", tok),
    }
  }

  fn ret_stmt(&mut self) -> Result<ast::Stmt<ast::Var>> {
    self.munch(Token::RETURN)?;
    Ok(ast::Stmt::Return(self.expr()?))
  }

  // Can be a struct or an enum.
  fn typedef(&mut self) -> Result<ast::Gstmt<ast::Var>> {
    let name = self.ident()?;
    self.munch(Token::EQUAL)?;
    let typ = self.typ()?;
    Ok(ast::Gstmt::Typedef { typ, name })
  }

  fn error<T, Q>(&self, message: T) -> Result<Q>
  where
    T: Into<String>,
  {
    let pos = self.lexer.position;
    err(format!(
      "Parse error at position [{}:{}]: \n {}",
      pos.line,
      pos.col,
      message.into()
    ))
  }

  fn arg_list(&mut self) -> Result<ast::Args<ast::Var>> {
    self.munch(Token::LPAREN)?;
    let mut result = Vec::new();
    if let Token::RPAREN = self.peek()? {
      self.skip()?;
      return Ok(result);
    }
    loop {
      match self.token()? {
        Token::Ident(ident) => {
          self.munch(Token::COLON)?;
          let typ = self.typ()?;
          result.push((typ, ident));
          match self.peek()? {
            Token::RPAREN => {
              self.skip()?;
              return Ok(result);
            }
            Token::COMMA => self.skip()?,
            tok => {
              return parse_error!(self, "Unknown token {:?} in argument list", tok);
            }
          }
        }
        tok => {
          return parse_error!(self, "Unknown token {:?} in argument list", tok);
        }
      }
    }
  }

  fn fun_defn(&mut self) -> Result<ast::Gstmt<ast::Var>> {
    let name = self.ident()?;
    let args = self.arg_list()?;
    let typ = self.typ()?;
    let body = self.expr()?;
    Ok(ast::Gstmt::Function {
      typ,
      name,
      args,
      body,
    })
  }

  fn field(&mut self) -> Result<(ast::Var, ast::Typ)> {
    let id = self.ident()?;
    self.munch(Token::COLON)?;
    let typ = self.typ()?;
    Ok((id, typ))
  }

  fn literal_fields(&mut self) -> Result<Vec<(ast::Var, ast::Expr<ast::Var>)>> {
    self.munch(Token::LBRACE)?;
    let mut fields = Vec::new();
    loop {
      match self.peek()? {
        Token::Ident(_) => {
          let id = self.ident()?;
          self.munch(Token::COLON)?;
          let expr = self.expr()?;
          fields.push((id, expr));
          match self.peek()? {
            Token::RBRACE => (),
            Token::COMMA | Token::SEMICOLON => self.skip()?,
            tok => {
              return parse_error!(self, "Expected closing brace or semicolon, got {:?}", tok);
            }
          }
        }
        Token::RBRACE => {
          self.skip()?;
          return Ok(fields);
        }
        tok => {
          return parse_error!(
            self,
            "Expected name of struct field in literal, got {:?}",
            tok
          );
        }
      }
    }
  }

  // Parse an argument list to a function call
  fn call_list(&mut self) -> Result<Vec<ast::Expr<ast::Var>>> {
    self.munch(Token::LPAREN)?;
    let mut args = Vec::new();
    match self.peek()? {
      Token::RPAREN => self.skip()?,
      _ => loop {
        args.push(self.expr()?);
        match self.token()? {
          Token::RPAREN => break,
          Token::COMMA => (),
          tok => return parse_error!(self, "Unexpected token {:?} in function call", tok),
        };
      },
    };
    Ok(args)
  }

  fn typ(&mut self) -> Result<ast::Typ> {
    match self.peek()? {
      Token::LPAREN => {
        // Tuple
        self.skip()?;
        let mut typs = Vec::new();
        if let Token::RPAREN = self.peek()? {
          self.skip()?;
          return Ok(ast::Typ::Tuple(typs));
        }
        loop {
          typs.push(self.typ()?);
          match self.token()? {
            Token::COMMA => (),
            Token::RPAREN => return Ok(ast::Typ::Tuple(typs)),
            tok => return parse_error!(self, "Expected comma or closing paren, got {:?}", tok),
          }
        }
      }
      Token::LBRACE => {
        // Anonymous struct
        self.skip()?;
        let mut fields = Vec::new();
        if let Token::RBRACE = self.peek()? {
          self.skip()?;
          return Ok(ast::Typ::Struct(fields));
        }
        loop {
          fields.push(self.field()?);
          match self.token()? {
            Token::COMMA => (),
            Token::RBRACE => return Ok(ast::Typ::Struct(fields)),
            tok => return parse_error!(self, "Expected comma or closing brace, got {:?}", tok),
          }
        }
      }
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
          Token::COLON => {
            // enum field
            let mut fields = Vec::new();
            loop {
              self.munch(Token::COLON)?;
              let t = self.typ()?;
              fields.push((id, t));
              match self.peek()? {
                Token::PIPE => {
                  self.skip()?;
                  id = self.ident()?;
                }
                _ => break ast::Typ::Enum(fields),
              }
            }
          }
          _ => ast::Typ::Alias(id),
        };
        loop {
          match self.peek()? {
            Token::Ident(_) => {
              let id = self.ident()?;
              next = ast::Typ::Composite(box next, id)
            }
            _ => return Ok(next),
          }
        }
      }
      tok => {
        return parse_error!(
          self,
          "Expected opening paren, brace, or identifier, got {:?}",
          tok
        )
      }
    }
  }

  fn ident(&mut self) -> Result<ast::Var> {
    match self.token()? {
      Token::Ident(string) => Ok(string),
      tok => parse_error!(self, "Could not match {:?} as ident", tok),
    }
  }

  fn expr(&mut self) -> Result<ast::Expr<ast::Var>> {
    self.lor_expr()
  }

  fn stmt(&mut self) -> Result<ast::Stmt<ast::Var>> {
    match self.peek()? {
      Token::BREAK => Ok(ast::Stmt::BREAK),
      Token::LET => self.let_stmt(),
      Token::RETURN => self.ret_stmt(),
      Token::LOOP => self.loop_stmt(), // These should be exprs, but aren't yet
      _ => Ok(ast::Stmt::Expr(self.expr()?)),
    }
  }

  fn loop_stmt(&mut self) -> Result<ast::Stmt<ast::Var>> {
    self.munch(Token::LOOP)?;
    Ok(ast::Stmt::Loop(self.expr()?))
  }

  // Todo: patterns
  // todo: mut
  fn let_stmt(&mut self) -> Result<ast::Stmt<ast::Var>> {
    self.munch(Token::LET)?;
    let name = self.ident()?;
    self.munch(Token::EQUAL)?;
    let value = self.expr()?;
    Ok(ast::Stmt::Let { name, value })
  }

  fn block_expr(&mut self) -> Result<ast::Expr<ast::Var>> {
    self.munch(Token::LBRACE)?;
    let mut stmts = Vec::new();
    // Todo: this got a bit messy. cleanup
    loop {
      if let Token::RBRACE = self.peek()? {
        self.skip()?;
        return Ok(ast::Expr::Statements(stmts));
      }
      let expr = self.stmt()?;
      match self.token()? {
        Token::SEMICOLON => {
          stmts.push(expr);
        }
        Token::RBRACE => {
          stmts.push(expr);
          return Ok(ast::Expr::Statements(stmts));
        }
        Token::EQUAL => {
          if let ast::Stmt::Expr(e) = expr {
            let lhs = e.to_lvalue()?;
            let rhs = self.expr()?;
            stmts.push(ast::Stmt::Assign {
              target: lhs,
              value: rhs,
            });
            match self.token()? {
              Token::SEMICOLON => continue,
              Token::RBRACE => return Ok(ast::Expr::Statements(stmts)),
              _ => (),
            }
          }
          return parse_error!(
            self,
            "Unexpected token {:?} after non-expression",
            Token::EQUAL
          );
        }
        tok => return parse_error!(self, "Unexpected token {:?} after expression", tok),
      };
    }
  }

  fn paren_expr(&mut self) -> Result<ast::Expr<ast::Var>> {
    self.munch(Token::LPAREN)?;
    let expr = self.expr()?;
    self.munch(Token::RPAREN)?;
    Ok(expr)
  }
}

// --------------------------------  PRECEDENCE  --------------------------------
macro_rules! expr_tier {
    ($name:ident, $next:ident, $($tok:pat = $op:expr),+) => {
        fn $name(&mut self) -> Result<ast::Expr<ast::Var>> {
          // println!("{}",stringify!($name));
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
          // println!("Pop {}",stringify!($name));
          Ok(expr)
        }
      };
  }

impl<'a> Parser<'a> {
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
  fn as_expr(&mut self) -> Result<ast::Expr<ast::Var>> {
    // println!("as_expr");
    let expr = self.with_expr()?;
    match self.peek() {
      Ok(Token::AS) => {
        self.skip()?;
        let target = self.typ()?;
        Ok(ast::Expr::AsExpression {
          expr: box expr,
          target,
        })
      }
      _ => Ok(expr),
    }
  }

  fn with_expr(&mut self) -> Result<ast::Expr<ast::Var>> {
    // println!("with_expr");
    let expr = self.unary_expr()?;
    match self.peek() {
      Ok(Token::WITH) => {
        self.skip()?;
        let fields = self.literal_fields()?;
        Ok(ast::Expr::WithExpression {
          expr: box expr,
          fields,
        })
      }
      _ => Ok(expr),
    }
  }

  fn unop(&mut self) -> Result<ast::UnOp> {
    match self.token()? {
      Token::MINUS => Ok(ast::UnOp::Sub),
      Token::LNOT => Ok(ast::UnOp::Not),
      tok => parse_error!(self, "Could not match token {:?} in unop", tok),
    }
  }

  fn unary_expr(&mut self) -> Result<ast::Expr<ast::Var>> {
    use ast::Expr::*;
    match self.peek() {
      Ok(Token::MINUS) | Ok(Token::LNOT) => {
        let op = self.unop()?;
        // println!("Push unary_expr");
        let expr = self.access_expr()?;
        // println!("Pop unary_expr ({:?})", expr);
        Ok(UnaryOp { op, rhs: box expr })
      }
      _ => {
        // println!("Push unary_expr");
        let result = self.access_expr();
        // println!("Pop unary_expr ({:?})", result);
        result
      }
    }
  }

  fn access_expr(&mut self) -> Result<ast::Expr<ast::Var>> {
    let expr = self.primary_expr()?;
    let mut results = Vec::new();
    loop {
      match self.peek() {
        Ok(Token::DOT) => {
          self.skip()?;
        }
        _ => break,
      };
      results.push(self.ident()?);
    }
    if results.is_empty() {
      Ok(expr)
    } else {
      Ok(ast::Expr::FieldAccess {
        expr: box expr,
        fields: results,
      })
    }
  }

  fn cond_expr(&mut self) -> Result<ast::Expr<ast::Var>> {
    self.munch(Token::IF)?;
    let condition = box self.expr()?;
    self.munch(Token::THEN)?;
    let t1 = box self.expr()?;
    self.munch(Token::ELSE)?;
    let t2 = box self.expr()?;
    Ok(ast::Expr::If { condition, t1, t2 })
  }

  fn primary_expr(&mut self) -> Result<ast::Expr<ast::Var>> {
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
      &Float(f) => {
        self.skip()?;
        Ok(ast::Expr::FloatLiteral(f))
      }
      Ident(_) => {
        let s = self.ident()?;
        match self.peek()? {
          Token::LPAREN => Ok(ast::Expr::Call {
            function: s,
            args: self.call_list()?,
          }),
          Token::LBRACE => Ok(ast::Expr::StructLiteral {
            name: s,
            fields: self.literal_fields()?,
          }),
          _ => Ok(ast::Expr::Variable(s)),
        }
      }
      IF => self.cond_expr(),
      LPAREN => self.paren_expr(),
      LBRACE => self.block_expr(),
      MINUS | LNOT => self.unary_expr(),
      tok => parse_error!(self, "Could not match {:?} in primary_expr", tok),
    }
  }
}
