#![feature(box_syntax)]
#![feature(box_patterns)]
#![feature(trait_alias)]
#![warn(clippy::all)]
#![allow(unknown_lints,dead_code,clippy::map_entry)]

mod args;
mod error;
mod lex;
mod parse;
mod ast;

use std::fs;
use std::fs::OpenOptions;
use std::io::prelude::*;
use std::io::BufReader;
use std::thread;
use std::time;
use std::fmt::{Debug};

fn reader(name: &str) -> std::io::BufReader<std::fs::File> {
  BufReader::new(fs::File::open(&name).unwrap_or_else(|_| panic!("File {} not found", name)))
}

/// Take a filename and parse it into an AST!
fn make_program(
  linkfile: Option<String>,
  filename: &str,
) -> std::result::Result<ast::Program, error::Error> {
  let file = reader(filename);
  match linkfile {
    None => {
      let mut parser = parse::Parser::new(lex::Lexer::new(file));
      parser.parse()
    }
    Some(linkname) => {
      let link = reader(&linkname);
      let mut link_parser = parse::Parser::new(lex::Lexer::new(link));
      let mut header = link_parser.parse()?;
      // if !tc::valid_header(&mut header) {
      //   return error::err("Function definition found in header.");
      // };
      let mut parser = parse::Parser::new_types(lex::Lexer::new(file), link_parser.types);
      let program = parser.parse()?;
      header.0.extend(program.0);
      Ok(header)
    }
  }
}

// Helper macro to time evaluating an expression (like a function call.)
macro_rules! time {
  ( $config:expr, $name:expr, $x:expr ) => {{
    let t1 = time::SystemTime::now();
    let result = $x;
    if $config.verbose {
      println!("{} [{}us]",$name,t1.elapsed().unwrap().as_micros())
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
      let filename = cfg.file.clone().unwrap();
      let link_file = cfg.link_file.clone();
      let program = time!(cfg,"Parse",make_program(link_file, &filename));
      let program = match program {
        Ok(prog) => prog,
        Err(e) => { eprintln!("{}", e); return 1 } // Parse failed!
      };

      // let valid_ast = time!(cfg,"Typecheck",tc::valid_ast(&program));

      // if !valid_ast {
      //   if cfg.dump_ast { println!("{:?}", program); }
      //   eprintln!("Invalid AST!");
      //   return 1; // Tc failed (sad!)
      // }

      if cfg.tc_only {
        println!("Pass typecheck.");
        return 0;
      }

      // let program = elab::elaborate(program);
      // if cfg.dump_ast {
      //   println!("{:?}",program);
      // }

      fn print_block_stream<T:Debug>(cfg: &args::Config, blocks: &[T]) {
        if cfg.dump_assem {
          for block in blocks.iter() {
            println!("{:?}\n",block);
          }
          println!("\n\n\n");
        }
      }

      0
      // let result = time!(cfg,"Evaluation - SSA");
      // println!("Result: {:?}", result2);
      // match cfg.emit {
      //   EmitTarget::Abstract => 0,
      //   _ => emit::emit_x86(&filename, result2).is_err() as i32
      // }
    })
    .unwrap();
  // Return the value from the child thread as the return value of the compiler.
  std::process::exit(child.join().expect("Couldn't join spawned thread"));
}
