use std::env;

pub enum EmitTarget {
  Abstract,
  X86,
  LLVM,
}

pub enum OptLevel {
  O0,
  O1,
  O2,
}

pub struct Config {
  pub verbose: bool,
  pub tc_only: bool,
  pub dump_parse: bool,
  pub dump_ast: bool,
  pub dump_ir: bool,
  pub dump_assem: bool,
  pub safe: bool,

  pub link_file: Option<String>,
  pub emit: EmitTarget,
  pub opt: OptLevel,

  pub file: Option<String>,
}

impl Config {
  fn default() -> Self {
    Config {
      verbose: false,
      tc_only: false,
      dump_parse: false,
      dump_ast: false,
      dump_ir: false,
      dump_assem: false,
      safe: true,

      link_file: None,
      emit: EmitTarget::X86,
      opt: OptLevel::O0,
      file: None,
    }
  }
}

pub fn parse_args() -> Config {
  let args: Vec<String> = env::args().collect();
  let mut config = Config::default();
  let mut index = 1;
  while index < args.len() {
    match args[index].as_str() {
      "-v" | "--verbose" => config.verbose = true,
      "--dump-parsing" => config.dump_parse = true,
      "--dump-ast" => config.dump_ast = true,
      "--dump-ir" => config.dump_ir = true,
      "--dump-assem" => config.dump_assem = true,
      "--unsafe" => config.safe = false,
      "-t" | "--typecheck-only" => config.tc_only = true,
      "-e" | "--emit" => {
        if index + 1 < args.len() {
          match args[index + 1].as_str() {
            "abs" => config.emit = EmitTarget::Abstract,
            "x86-64" => config.emit = EmitTarget::X86,
            "llvm" => config.emit = EmitTarget::LLVM,
            other => {
              panic!(format!("Unkown emit type : {}", other.to_string()));
            }
          };
          index += 1;
        } else {
          panic!("Expected emit type");
        };
      }
      "-ex86-64" => config.emit = EmitTarget::X86,
      "-O1" => config.opt = OptLevel::O1,
      "-O2" => config.opt = OptLevel::O2,
      "-l" | "-link" => {
        if index + 1 < args.len() {
          config.link_file = Some(args[index + 1].clone());
          index += 1;
        } else {
          panic!("Expected file after link param.");
        };
      }
      file => {
        if let Some('-') = file.chars().nth(0) {
        } else {
          config.file = Some(file.to_string())
        }
      }
    };
    index += 1;
  }

  if config.file.is_none() {
    panic!("Expected file input");
  }

  // if config.link_file.is_none() {
  //   config.link_file = Some(String::from("../runtime/15411-l3.h0"))
  // }

  config
}
