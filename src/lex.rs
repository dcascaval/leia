// Leia Compiler
//! Lexer
/// Supports UTF-8 encoded files, and will ignore any characters that aren't, though the
/// lexer will fail to match any unidentified characters.
/// 
/// Uses a recursive matching on a byte stream rather than directly attempting 
/// regular expressions. This allows us to make the control flow a little clearer 
/// and define custom error handling and recovery. 

// Todo: 
// Add positional information
    // Line, column.
    // This will aid the parser in generating spans.

// Add reasonable-looking lexer errors, and handle them correctly.
    // Failed to parse number, unrecognized symbol, things like that.

// Add attempted recovery.
    // This follows the general principle that if a token fails to lex, 
    // that's fine, and we'll insert an 'INVALID' token in the stream.
    // The parser will do one of two things: 
    // - Attempt to skip it (inserting INVALID) and parse the rest of the expression.
    //   This is valid when we are parsing some large expression that we know the overall
    //   structure of and can ignore a component. This works particularly well in constructs
    //   with 'separators' (e.g. semicolons, pipes, commas).
    // 
    // - Treat the current parse scope as invalid and return an INVALID ast node. 
    //   This is the path when we don't know what we're parsing (i.e looking for a new statement
    //   beginning) and the results are completely invalidated. Here we usually jump to the next 
    //  'delimiter' (e.g. brace, parens, etc.) such that it matches what we've parsed so far.
    //   If this horrendously fails, it likely means the user has mismatched delimiters in some
    //   unrecoverable way, and we will attempt to error accordingly.

use crate::error::{err, errs, Error, Result};

use std::char;
use std::borrow::Cow;
use std::iter::Peekable;

#[derive(PartialEq, Debug, Clone)]
pub enum Token {
  Ident(String),
  Number(i64),
  Float(f64),
  Boolean(bool),

  // Syntactic
  LPAREN,
  RPAREN,
  LBRACE,
  RBRACE,
  COMMA,
  SEMICOLON,
  COLON,
  PIPE,

  // Operators
  PLUS,
  MINUS,
  TIMES,
  DIV,
  MOD,
  LNOT,
  LAND,
  LOR,

  // Comparisons
  LT,
  LTE,
  GT,
  GTE,
  EQEQ,
  NOTEQ,

  // Assignments
  EQUAL,

  // Control flow
  LOOP,
  IF,
  ELSE,
  MATCH,
  RETURN,
  BREAK,

  // Keywords 
  FUNCTION,
  TYPE,
  STRUCT, 
  AS,
}

pub struct Lexer<'a> {
  stream: Peekable<std::str::Chars<'a>>
}

impl<'a> Lexer<'a> {

  pub fn new(buf: &'a Cow<str>) -> Self {
    let chars = buf.chars(); 
    let peek = chars.peekable();
    Lexer {
      stream: peek
    }
  }

  fn skip(&mut self) {
    self.stream.next();
  }

  /// Advance the stream and return the token.
  fn single(&mut self, tok: Token) -> Result<Token> {
    self.skip();
    Ok(tok)
  }

  /// Peek at the stream. This will return the same value if called multiple times.
  fn current(&mut self) -> Result<char> {
    if let Some(&byte) = self.stream.peek() {
      return Ok(byte as char);
    }
    Err(Error::EOF)
  }

  /// Analogous to 'drop'. Advances stream while predicate is met.
  fn skip_while<F: Fn(char) -> bool>(&mut self, pred: F) -> Result<()> {
    let mut ch = self.current()?;
    while pred(ch) {
      self.stream.next();
      if let Some(&b) = self.stream.peek() {
        ch = b as char;
      } else {
        break;
      }
    }
    Ok(())
  }

  /// Like skip_while, except that it returns what it skipped.
  fn take_while<F: Fn(char) -> bool>(&mut self, pred: F) -> Result<String> {
    let mut buffer = String::new();
    buffer.push(self.current()?);
    self.skip();
    let mut ch = self.current()?;
    while pred(ch) {
      buffer.push(ch);
      self.skip();
      if let Some(&b) = self.stream.peek() {
        ch = b as char;
      } else {
        break;
      }
    }
    Ok(buffer)
  }

  // Advance the iterator until the next line is done.
  fn line_comment(&mut self) -> Result<()> {
    self.skip_while(|ch| ch != '\n')?;
    self.skip();
    Ok(())
  }

  fn slash(&mut self) -> Result<Token> {
    self.skip();
    match self.current()? {
      '/' => {
        self.line_comment()?;
        self.token()
      }
      _ => Ok(Token::DIV),
    }
  }

  fn amp(&mut self) -> Result<Token> {
    self.skip();
    match self.current()? {
      '&' => self.single(Token::LAND),
      _ => err("Found single ampersand."),
    }
  }

  fn pipe(&mut self) -> Result<Token> {
    self.skip();
    match self.current()? {
      '|' => self.single(Token::LOR),
      _ => Ok(Token::PIPE),
    }
  }

  fn maybe_equals(&mut self, t: Token, equals: Token) -> Result<Token> {
    self.skip();
    match self.current()? {
      '=' => self.single(equals),
      _ => Ok(t),
    }
  }

  fn number(&mut self) -> Result<Token> { 
    let buffer = self.take_while(|c| char::is_numeric(c) || c == '_' || c == '.' || c == 'e')?;
    match buffer.parse::<i64>() {
      Ok(n) => Ok(Token::Number(n)),
      Err(_) => Ok(Token::Float(buffer.parse::<f64>().unwrap())),
    }
  }

  fn literal(&mut self) -> Result<Token> {
    if let Ok(ident) = self.take_while(|ch| ch.is_alphanumeric() || ch == '_') {
      return match ident.as_str() {
        // Keywords:
        "fn" => Ok(Token::FUNCTION),
        "type" => Ok(Token::TYPE),
        "loop" => Ok(Token::LOOP),
        "break" => Ok(Token::BREAK),
        "if" => Ok(Token::IF),
        "else" => Ok(Token::ELSE),
        "return" => Ok(Token::RETURN), 
        "match" => Ok(Token::MATCH),
        "as" => Ok(Token::AS),
        "true" => Ok(Token::Boolean(true)),
        "false" => Ok(Token::Boolean(false)),
        _tok => Ok(Token::Ident(ident)),
      };
    }
    // Todo: add error
    err("Unexpectedly reached end of file while trying to parse: ")
  }

  pub fn token(&mut self) -> Result<Token> {
    let c = self.current()?;
    if c.is_whitespace() {
      self.skip_while(|ch| ch.is_whitespace())?;
      self.token()
    } else {
      match c {
        '}' => self.single(Token::RBRACE),
        '{' => self.single(Token::LBRACE),
        '(' => self.single(Token::LPAREN),
        ')' => self.single(Token::RPAREN),
        ',' => self.single(Token::COMMA),
        ':' => self.single(Token::COLON),
        ';' => self.single(Token::SEMICOLON),
        '*' => self.single(Token::TIMES),
        '-' => self.single(Token::MINUS),
        '+' => self.single(Token::PLUS),
        '%' => self.single(Token::MOD),
        '=' => self.maybe_equals(Token::EQUAL, Token::EQEQ),
        '!' => self.maybe_equals(Token::LNOT, Token::NOTEQ),
        '<' => self.maybe_equals(Token::LT, Token::LTE),
        '>' => self.maybe_equals(Token::GT, Token::GTE),
        '|' => self.pipe(), 
        '&' => self.amp(),
        '/' => self.slash(),
        '0'..='9' => self.number(),
        'a'..='z' | 'A'..='Z' | '_' => self.literal(),
        _ => errs(format!("Lexer failed to match character {:?}", c)),
      }
    }
  }
}
