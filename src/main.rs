use std::{env, fs::File, path, io::{self, Read, Write, BufWriter, BufReader, stdin, stdout}};
#[macro_use] extern crate educe;

use lexer::lines;
use parser::{parse, enums::{Value, Number}};
use evaluator::*;

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

    pub fn run<S: Read + 'static>(&mut self, script: S) {
        let lines = lines(script);
        let scope = parse(lines);
        match scope {
            Ok(s) => {
                
                let mut evaluator = Evaluator::new(&mut self.input, &mut self.output, self.debug);
                let result = evaluator.eval_outside_scope(&s);
                match result {
                    Ok(Value::Nil) => std::process::exit(0),
                    Ok(Value::Number(Number::Integer(code))) => std::process::exit(code),
                    Ok(_) => (),
                    Err(e) => println!("{:?}", e),
                }
            }
            Err(e) => self.output.write_fmt(format_args!("{:?}", e)).unwrap(),
        };
    }

}

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

    
    Interpreter::new(stdin(), stdout(), false).run(file);
}
