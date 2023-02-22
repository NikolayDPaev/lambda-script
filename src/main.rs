use std::{
    env,
    fs::File,
    io::{self, stdin, stdout, BufReader, BufWriter, Read, Write},
    path::{self, PathBuf},
};
#[macro_use]
extern crate educe;

use evaluator::errors::process_evaluator_error;
use evaluator::*;
use lexer::lines;
use parser::{
    enums::{Number, Value},
    Parser,
};

mod evaluator;
mod lexer;
mod parser;
#[cfg(test)]
mod tests;

pub struct Interpreter<R: Read, W: Write> {
    input: BufReader<R>,
    output: BufWriter<W>,
    debug: bool,
}

impl<R, W> Interpreter<R, W>
where
    R: Read,
    W: Write,
{
    pub fn new(input: R, output: W, debug: bool) -> Interpreter<R, W> {
        Interpreter {
            input: BufReader::new(input),
            output: BufWriter::new(output),
            debug,
        }
    }

    pub fn run<S: Read + 'static>(&mut self, script: S, file_path: PathBuf) -> i32 {
        let lines = lines(script);
        let mut parser = Parser::new(lines, file_path);
        match parser.parse_outside_scope() {
            Ok(s) => {
                let mut evaluator = Evaluator::new(&mut self.input, &mut self.output, self.debug);
                let result = evaluator.eval_outside_scope(&s);
                match result {
                    Ok(Value::Nil) => return 0,
                    Ok(Value::Number(Number::Integer(code))) => return code,
                    Ok(_) => (),
                    Err(e) => self
                        .output
                        .write_fmt(format_args!("{}", process_evaluator_error(e, &parser.names)))
                        .unwrap(),
                }
            }
            Err(e) => self
                .output
                .write_fmt(format_args!("{}", e.get_message()))
                .unwrap(),
        };
        return 1;
    }
}

fn main() {
    if let Ok(manifest_dir) = env::var("CARGO_MANIFEST_DIR") {
        let mut path = path::PathBuf::from(manifest_dir);
        path.push("resources");
    }

    let mut debug = false;
    let filename = match env::args().nth(1) {
        Some(arg) if arg == "-d" || arg == "--debug" => {
            debug = true;
            match env::args().nth(2) {
                Some(arg) => arg,
                _ => std::process::exit(1),
            }
        }
        Some(arg) if arg == "-h" || arg == "--help" => {
            println!("Usage: lambda-script.exe <file>");
            println!("Options: --debug (-d), --help (-h)");
            std::process::exit(0)
        }
        Some(arg) => arg,
        _ => {
            println!("Usage: lambda-script.exe <file>");
            std::process::exit(1)
        }
    };
    let canonical_path = match PathBuf::from(filename.clone()).canonicalize() {
        io::Result::Ok(path) => path,
        io::Result::Err(error) => {
            println!("Invalid path: {:?}. Error: {}", filename, error.to_string());
            std::process::exit(1)
        }
    };
    let file = match File::open(canonical_path.clone()) {
        io::Result::Ok(file) => file,
        io::Result::Err(error) => {
            println!(
                "Cannot open file: {:?}. Error: {}",
                filename,
                error.to_string()
            );
            std::process::exit(1)
        }
    };
    let code = Interpreter::new(stdin(), stdout(), debug).run(file, canonical_path);
    std::process::exit(code);
}
