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
use crate::error::{err, Error, Result};

use std::borrow::Cow;
use std::char;
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
  DOT,

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
  THEN,
  ELSE,
  MATCH,
  RETURN,
  BREAK,

  // Keywords
  FUNCTION,
  TYPE,
  STRUCT,
  AS,
  LET,
  WITH, // Reserve
}

pub struct Lexer<'a> {
  pub position: Position,
  stream: Peekable<std::str::Chars<'a>>,
}

#[derive(Copy, Clone)]
pub struct Position {
  pub line: u32,
  pub col: u32,
}

#[derive(Copy, Clone)]
pub struct Span {
  pub start: Position,
  pub end: Position,
}

macro_rules! lex_error {
  ($lexer:ident, $($arg:tt),+) => {
    Lexer::error($lexer,format!($($arg),+))
  };
}

impl<'a> Lexer<'a> {
  pub fn new(buf: &'a Cow<str>) -> Self {
    let chars = buf.chars();
    let peek = chars.peekable();
    Lexer {
      position: Position { line: 1, col: 0 },
      stream: peek,
    }
  }

  fn skip(&mut self) {
    match self.stream.next() {
      Some('\n') => {
        self.position.line += 1;
        self.position.col = 0;
      }
      Some(_) => {
        self.position.col += 1;
      }
      None => (),
    }
  }

  fn error<T, Q>(&mut self, message: T) -> Result<Q>
  where
    T: Into<String>,
  {
    err(format!(
      "Lexer Error at position [{}:{}] : {}",
      self.position.line,
      self.position.col,
      message.into()
    ))
  }

  fn peek(&mut self) -> Option<&char> {
    self.stream.peek()
  }

  /// Advance the stream and return the token.
  fn single(&mut self, tok: Token) -> Result<Token> {
    self.skip();
    Ok(tok)
  }

  /// Peek at the stream. This will return the same value if called multiple times.
  fn current(&mut self) -> Result<char> {
    if let Some(&c) = self.peek() {
      return Ok(c);
    }
    Err(Error::EOF)
  }

  /// Analogous to 'drop'. Advances stream while predicate is met.
  fn skip_while<F: Fn(char) -> bool>(&mut self, pred: F) -> Result<()> {
    let mut ch = self.current()?;
    while pred(ch) {
      self.skip();
      if let Some(&b) = self.peek() {
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
      if let Some(&b) = self.peek() {
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
      _ => self.error("Found single ampersand"),
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
    use Token::*;
    if let Ok(ident) = self.take_while(|ch| ch.is_alphanumeric() || ch == '_') {
      return Ok(match ident.as_str() {
        // Keywords:
        "fn" => FUNCTION,
        "type" => TYPE,
        "loop" => LOOP,
        "break" => BREAK,
        "if" => IF,
        "else" => ELSE,
        "return" => RETURN,
        "match" => MATCH,
        "as" => AS,
        "with" => WITH,
        "let" => LET,
        "then" => THEN,
        "true" => Boolean(true),
        "false" => Boolean(false),
        _tok => Ident(ident),
      });
    }
    self.error("Unexpectedly reached end of file while trying to parse.")
  }

  pub fn token(&mut self) -> Result<Token> {
    let c = self.current()?;
    if c.is_whitespace() {
      self.skip_while(|ch| ch.is_whitespace())?;
      self.token()
    } else {
      use Token::*;
      match c {
        '}' => self.single(RBRACE),
        '{' => self.single(LBRACE),
        '(' => self.single(LPAREN),
        ')' => self.single(RPAREN),
        ',' => self.single(COMMA),
        ':' => self.single(COLON),
        ';' => self.single(SEMICOLON),
        '*' => self.single(TIMES),
        '-' => self.single(MINUS),
        '+' => self.single(PLUS),
        '%' => self.single(MOD),
        '.' => self.single(DOT),
        '=' => self.maybe_equals(EQUAL, EQEQ),
        '!' => self.maybe_equals(LNOT, NOTEQ),
        '<' => self.maybe_equals(LT, LTE),
        '>' => self.maybe_equals(GT, GTE),
        '|' => self.pipe(),
        '&' => self.amp(),
        '/' => self.slash(),
        '0'..='9' => self.number(),
        'a'..='z' | 'A'..='Z' | '_' => self.literal(),
        _ => lex_error!(self, "Lexer failed to match character {:?}", c),
      }
    }
  }
}
