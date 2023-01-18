use std::{env, fs::File, path, io};
#[macro_use] extern crate educe;

use lexer::lines;
use parser::{parse, enums::{Value, Number}};
use evaluator::*;

mod evaluator;
mod lexer;
mod parser;

fn main() {
    if let Ok(manifest_dir) = env::var("CARGO_MANIFEST_DIR") {
        let mut path = path::PathBuf::from(manifest_dir);
        path.push("resources");
    }

    let arg = match env::args().nth(1) {
        Some(arg) => arg,
        _ => {
            println!("Usage: lambda-script.exe <file>");
            std::process::exit(1)
        }
    };

    let file = match File::open(arg) {
        io::Result::Ok(file) => file,
        _ => {
            println!("Cannot open file");
            std::process::exit(1)
        }
    };

    let lines = lines(file);
    let scope = parse(lines);
    match scope {
        Ok(s) => {
            //println!("{:?}", s);
            let mut evaluator = Evaluator::new();
            let result = evaluator.eval_outside_scope(&s);
            match result {
                Ok(Value::Nil) => std::process::exit(0),
                Ok(Value::Number(Number::Integer(code))) => std::process::exit(code),
                Ok(_) => (),
                Err(e) => println!("{:?}", e),
            }
        }
        Err(e) => println!("{:?}", e),
    };
}
