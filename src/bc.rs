// Leia Compiler
//! Bytecode Representation
//  This is probably some basic-block-like representation of code, where minor
//  optimizations can be done on a program (particularly in loop bodies) before
//  it is compiled into a single stream of code and interpreted.

use crate::ast;
use std::fmt::Debug;

struct Location {
  index: u32,
  size: u32,
}

struct FunctionIndex(usize);
struct BlockIndex(usize);

enum Operation {
  Move {
    dest: Location,
    src: Location,
  },
  Unop {
    op: ast::UnOp,
    dest: Location,
    src: Location,
  },
  Binop {
    op: ast::BinOp,
    dest: Location,
    src1: Location,
    src2: Location,
  },
}

enum Instruction {
  Operation(Operation),
  Branch {
    src: Location,
    t1: BlockIndex,
    t2: BlockIndex,
  },
  Call {
    name: FunctionIndex,
    dest: Location,
    srcs: Vec<Location>,
  },
  Jump {
    target: BlockIndex,
  },
  Return,
}

struct Context {}

type Program = Vec<Vec<Instruction>>;

fn cg_ast<T: Debug>(_program: ast::Program<T>) -> Program {
  unimplemented!()
}

impl Context {
  fn new(self) -> Self {
    unimplemented!()
  }
}
