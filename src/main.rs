use std::{env, fs::File, path};

use lexer::lines;
use parser::parse;

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
        Ok(s) => println!("{:?}", s),
        Err(e) => println!("{:?}", e),
    };
}
