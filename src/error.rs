// Leia Compiler
//! Compiler Errors
// Author: Dan Cascaval <dcascava@andrew.cmu.edu>

use std::fmt::{Display, Formatter};
use std::io;
use std::result;

pub type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
pub enum Error {
  Message(String),
  EOF,
}

/// Shorthand to make an error with a message
fn msg(s: &str) -> Error {
  Error::Message(s.to_string())
}

/// Shorthand to make an `std::result::Error` with a message
pub fn err<T>(s: &str) -> Result<T> {
  Err(Error::Message(s.to_string()))
}

/// Shorthand to use an owned string, such as the result of a formatter.
pub fn errs<T>(s: String) -> Result<T> {
  Err(Error::Message(s))
}

// Implement the traits we need to use this in the lexer & parser.

impl From<io::Error> for Error {
  fn from(_error: io::Error) -> Self {
    use std::error::Error;
    msg(_error.description())
  }
}

impl<'a> From<&'a Error> for Error {
  fn from(_error: &'a Error) -> Self {
    match _error {
      Error::Message(s) => Error::Message(s.clone()),
      Error::EOF => Error::EOF,
    }
  }
}

impl Display for Error {
  fn fmt(&self, fmt: &mut Formatter) -> result::Result<(), std::fmt::Error> {
    match *self {
      Error::Message(ref msg) => write!(fmt, "{}", &msg),
      Error::EOF => write!(fmt, "end of file"),
    }
  }
}
