use std::{
    env,
    fs::File,
    io::{self, stdin, stdout, BufReader, BufWriter, Read, Write},
    path,
};
#[macro_use]
extern crate educe;

use evaluator::errors::process_evaluator_error;
use evaluator::*;
use lexer::lines;
use parser::{
    enums::{Number, Value},
    parse,
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

    pub fn run<S: Read + 'static>(&mut self, script: S, filename: &str) -> i32 {
        let lines = lines(script);
        let scope = parse(lines, filename);
        match scope {
            Ok(s) => {
                //println!("{:?}", s);
                let mut evaluator = Evaluator::new(&mut self.input, &mut self.output, self.debug);
                let result = evaluator.eval_outside_scope(&s);
                match result {
                    Ok(Value::Nil) => return 0,
                    Ok(Value::Number(Number::Integer(code))) => return code,
                    Ok(_) => (),
                    Err(e) => self
                        .output
                        .write_fmt(format_args!("{}", process_evaluator_error(e)))
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
            std::process::exit(0)
        }
        Some(arg) => arg,
        _ => std::process::exit(1),
    };

    let file = match File::open(filename.clone()) {
        io::Result::Ok(file) => file,
        io::Result::Err(error) => {
            println!("Cannot open file: {:?} with error: {:?}", filename, error);
            std::process::exit(1)
        }
    };
    let code = Interpreter::new(stdin(), stdout(), debug).run(file, &filename);
    std::process::exit(code);
}
