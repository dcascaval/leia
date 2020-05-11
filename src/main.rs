#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(trait_alias)]
#![feature(or_patterns)]
#![warn(clippy::all)]
#![allow(unknown_lints, dead_code, clippy::map_entry)]

mod args;
mod ast;
mod bc;
mod error;
mod intern;
mod interpret;
mod lex;
mod parse;
mod tc;
mod vm;

use std::fs;
use std::fs::OpenOptions;
use std::io::prelude::*;
use std::io::BufReader;
use std::thread;
use std::time;

fn reader(name: &str) -> std::io::BufReader<std::fs::File> {
  BufReader::new(fs::File::open(&name).unwrap_or_else(|_| panic!("File {} not found", name)))
}

/// Take a filename and parse it into an AST!
fn make_program(filename: &str) -> std::result::Result<ast::Program<ast::Var>, error::Error> {
  let mut file = reader(filename);
  file.fill_buf()?;
  let buf = String::from_utf8_lossy(file.buffer());
  let mut parser = parse::Parser::new(lex::Lexer::new(&buf));
  parser.parse()
}

// Helper macro to time evaluating an expression (like a function call.)
macro_rules! time {
  ( $config:expr, $name:expr, $x:expr ) => {{
    let t1 = time::SystemTime::now();
    let result = $x;
    if $config.verbose {
      println!("{} [{}us]", $name, t1.elapsed().unwrap().as_micros())
    }
    result
  }};
}

fn write_line(str: String) {
  let mut file = OpenOptions::new()
    .write(true)
    .create(true)
    .append(true)
    .open("log.txt")
    .unwrap();

  if let Err(e) = writeln!(file, "{}\n", str) {
    eprintln!("Couldn't write to file: {}", e);
  }
}

fn main() {
  let cfg = args::parse_args();

  let child = thread::Builder::new()
    .stack_size(64 * 1024 * 1024)
    .spawn(move || {
      let filename = cfg.file.clone();
      let program = time!(cfg, "Parse", make_program(&filename));
      let program = match program {
        Ok(prog) => prog,
        Err(e) => {
          eprintln!("{}", e);
          return 1;
        } // Parse failed!
      };

      if cfg.dump_ast {
        println!("{:?}", program);
      }

      match tc::typecheck(&program) {
        Ok(_) => println!("Pass typecheck."),
        Err(m) => eprintln!("Type error: {}", m),
      }

      let (interner, program) = intern::convert(program);
      println!("{}", interpret::eval(interner, program));
      0
    })
    .unwrap();
  // Return the value from the child thread as the return value of the compiler.
  std::process::exit(child.join().expect("Couldn't join spawned thread"));
}
