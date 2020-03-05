use std::env;

pub struct Config {
  pub verbose: bool,
  pub tc_only: bool,
  pub dump_parse: bool,
  pub dump_ast: bool,
  pub file: String,
}

impl Config {
  fn default() -> Self {
    Config {
      verbose: false,
      tc_only: false,
      dump_parse: false,
      dump_ast: false,
      file: String::default(),
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
      "-t" | "--typecheck-only" => config.tc_only = true,
      file => {
        if let Some('-') = file.chars().nth(0) {
        } else {
          config.file = file.to_string()
        }
      }
    };
    index += 1;
  }

  if config.file.len() == 0 {
    panic!("Expected file input");
  }

  config
}
