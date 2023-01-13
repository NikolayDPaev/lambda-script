use std::{env, fs::File, path};
#[macro_use] extern crate educe;

use lexer::lines;
use parser::parse;
use evaluator::*;

mod evaluator;
mod lexer;
mod parser;

fn main() {
    if let Ok(manifest_dir) = env::var("CARGO_MANIFEST_DIR") {
        let mut path = path::PathBuf::from(manifest_dir);
        path.push("resources");
    }

    let file = File::open("test.ls").expect("no such file");

    let lines = lines(file);
    let scope = parse(lines);
    match scope {
        Ok(s) => {
            println!("{:?}", s);
            let mut evaluator = Evaluator::new();
            println!("{:?}", evaluator.eval_outside_scope(&s));
            
        }
        Err(e) => println!("{:?}", e),
    };
}
