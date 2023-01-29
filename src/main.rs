use std::{env, fs::File, path, io::{self, Read, Write, BufWriter, BufReader, stdin, stdout}};
#[macro_use] extern crate educe;

use lexer::lines;
use parser::{parse, enums::{Value, Number}, errors::process_parser_error};
use evaluator::*;
use evaluator::errors::process_evaluator_error;

#[cfg(test)]
mod tests;
mod evaluator;
mod lexer;
mod parser;

pub struct Interpreter<R : Read, W: Write> {
    input: BufReader<R>,
    output: BufWriter<W>,
    debug: bool,
}

impl<R, W> Interpreter<R, W> where R: Read, W: Write {
    pub fn new(input: R, output: W, debug: bool) -> Interpreter<R, W> {
        Interpreter { input: BufReader::new(input), output: BufWriter::new(output), debug }
    }

    pub fn run<S: Read + 'static>(&mut self, script: S) -> i32 {
        let lines = lines(script);
        let scope = parse(lines);
        match scope {
            Ok(s) => {
                //println!("{:?}", s);
                let mut evaluator = Evaluator::new(&mut self.input, &mut self.output, self.debug);
                let result = evaluator.eval_outside_scope(&s);
                match result {
                    Ok(Value::Nil) => return 0,
                    Ok(Value::Number(Number::Integer(code))) => return code,
                    Ok(_) => (),
                    Err(e) => self.output.write_fmt(format_args!("{}", process_evaluator_error(e))).unwrap(),
                }
            }
            Err(e) => self.output.write_fmt(format_args!("{}", process_parser_error(e))).unwrap(),
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
    let arg = match env::args().nth(1) {
        Some(arg) if arg == "-d" || arg == "--debug" => {
            debug = true;
            match env::args().nth(2) {
                Some(arg) => arg,
                _ => std::process::exit(1)
            }
        },
        Some(arg) if arg == "-h" || arg == "--help" => {
            println!("Usage: lambda-script.exe <file>");
            std::process::exit(0)
        },
        Some(arg) => arg,
        _ => std::process::exit(1)
    };

    let file = match File::open(arg) {
        io::Result::Ok(file) => file,
        _ => {
            println!("Cannot open file");
            std::process::exit(1)
        }
    };
    let code = Interpreter::new(stdin(), stdout(), debug).run(file);
    std::process::exit(code);
}
