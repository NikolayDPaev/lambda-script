pub mod enums;
#[cfg(test)]
mod tests;

use enums::*;
use std::io::{BufRead, BufReader, Read};

macro_rules! read_while_alphanumeric_into {
    ($iter:expr, $string:expr) => {
        while let Some(ch) = $iter.peek() {
            if !ch.is_alphanumeric() && *ch != '.' && *ch != '_' {
                break;
            } else {
                $string.push(*ch);
                $iter.next();
            }
        }
    };
}

macro_rules! read_until_quotes_into {
    ($iter:expr, $string:expr) => {
        while let Some(ch) = $iter.next() {
            if ch == '\"' {
                break;
            } else {
                $string.push(ch);
            }
        }
    };
}

macro_rules! read_until_single_quotes_into {
    ($iter:expr, $string:expr) => {
        while let Some(ch) = $iter.next() {
            if ch == '\'' {
                break;
            } else {
                $string.push(ch);
            }
        }
    };
}

#[derive(Debug)]
pub struct Line {
    pub number: u32,
    pub indentation: u16,
    pub tokens: Vec<Token>,
}

impl Line {
    pub fn new(line_str: String, line_num: u32) -> Self {
        let mut indentation: u16 = 0;
        let mut iter = line_str.chars().peekable();

        while let Some(ch) = iter.peek() {
            if *ch == ' ' {
                indentation += 1;
                iter.next();
            } else if *ch == '\t' {
                indentation += 4;
                iter.next();
            } else {
                break;
            }
        }

        let mut tokens: Vec<Token> = Vec::new();

        while let Some(ch) = iter.next() {
            if ch.is_whitespace() {
                continue;
            } else if ch == '#' {
                break;
            } else if ch.is_alphabetic() {
                let mut string = String::new();
                string.push(ch);
                read_while_alphanumeric_into!(iter, string);
                if string == "if" {
                    tokens.push(Token::If);
                } else if string == "then" {
                    tokens.push(Token::Then);
                } else if string == "else" {
                    tokens.push(Token::Else);
                } else if string == "true" {
                    tokens.push(Token::True);
                } else if string == "false" {
                    tokens.push(Token::False);
                } else if string == "impure" {
                    tokens.push(Token::Impure);
                } else if string == "cons" {
                    tokens.push(Token::Cons);
                } else if string == "left" {
                    tokens.push(Token::Left);
                } else if string == "right" {
                    tokens.push(Token::Right);
                } else if string == "empty" {
                    tokens.push(Token::Empty);
                } else if string == "nil" {
                    tokens.push(Token::Nil);
                } else if string == "import" {
                    tokens.push(Token::Import);
                } else if string == "once" {
                    tokens.push(Token::Once);
                } else if string == "read" {
                    tokens.push(Token::Read);
                } else if string == "print" {
                    tokens.push(Token::Print);
                } else {
                    tokens.push(Token::Name(string));
                }
            } else if ch.is_numeric() {
                let mut string = String::new();
                string.push(ch);
                read_while_alphanumeric_into!(iter, string);
                tokens.push(Token::Number(string));
            } else if ch == '\"' {
                let mut string = String::new();
                read_until_quotes_into!(iter, string);
                tokens.push(Token::Str(string));
            } else if ch == '\'' {
                let mut string = String::new();
                read_until_single_quotes_into!(iter, string);
                tokens.push(Token::Char(string));
            } else if ch == '(' {
                tokens.push(Token::LeftBracket);
            } else if ch == ')' {
                tokens.push(Token::RightBracket);
            } else if ch == '[' {
                tokens.push(Token::LeftBoxBracket);
            } else if ch == ']' {
                tokens.push(Token::RightBoxBracket);
            } else if ch == '-' {
                if matches!(iter.peek(), Some(x) if *x == '>') {
                    iter.next();
                    tokens.push(Token::Arrow);
                } else {
                    tokens.push(Token::Operation(Op::Minus));
                }
            } else if ch == '=' {
                if matches!(iter.peek(), Some(x) if *x == '=') {
                    iter.next();
                    tokens.push(Token::Operation(Op::Eq));
                } else {
                    tokens.push(Token::Assignment);
                }
            } else if ch == '<' {
                if matches!(iter.peek(), Some(x) if *x == '=') {
                    iter.next();
                    tokens.push(Token::Operation(Op::LEq));
                } else {
                    tokens.push(Token::Operation(Op::Lt));
                }
            } else if ch == '>' {
                if matches!(iter.peek(), Some(x) if *x == '=') {
                    iter.next();
                    tokens.push(Token::Operation(Op::GEq));
                } else {
                    tokens.push(Token::Operation(Op::Gt));
                }
            } else if ch == '*' {
                if matches!(iter.peek(), Some(x) if *x == '*') {
                    iter.next();
                    tokens.push(Token::Operation(Op::Exponentiation));
                } else {
                    tokens.push(Token::Operation(Op::Multiplication));
                }
            } else if ch == ',' {
                tokens.push(Token::Comma);
            } else if ch == '!' {
                if matches!(iter.peek(), Some(x) if *x == '=') {
                    iter.next();
                    tokens.push(Token::Operation(Op::NEq));
                } else {
                    tokens.push(Token::Operation(Op::Negation));
                }
            } else if ch == '&' {
                tokens.push(Token::Operation(Op::And));
            } else if ch == '|' {
                tokens.push(Token::Operation(Op::Or));
            } else if ch == '^' {
                tokens.push(Token::Operation(Op::Xor));
            } else if ch == '+' {
                tokens.push(Token::Operation(Op::Plus));
            } else if ch == '/' {
                if matches!(iter.peek(), Some(x) if *x == '/') {
                    iter.next();
                    tokens.push(Token::Operation(Op::IntDivision));
                } else {
                    tokens.push(Token::Operation(Op::Division));
                }
            } else if ch == '%' {
                tokens.push(Token::Operation(Op::Modulo));
            }
        }

        Line {
            number: line_num,
            indentation,
            tokens,
        }
    }
}

pub type LinesIterator = Box<dyn Iterator<Item = Result<Line, std::io::Error>>>;

pub fn lines<T: Read + 'static>(reader: T) -> LinesIterator {
    let buf_reader = BufReader::new(reader);
    Box::new(
        buf_reader
            .lines()
            .enumerate()
            .filter_map(|(line_num, result)| {
                match result.map(|line| Line::new(line, line_num as u32 + 1)) {
                    Ok(line) if line.tokens.is_empty() => None,
                    line_res @ _ => Some(line_res)
                }
            }),
    )
}
