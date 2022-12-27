use std::{io::{self, Read, BufReader, BufRead}};

pub enum LexerError {
    IoError(io::Error)
}

#[derive(Debug, PartialEq, Eq)]
pub enum Token {
    Name(String),
    Str(String),
    Char(String),
    Number(String),
    Operation(Op),
    If,
    Else,
    Then,
    Assignment,
    Arrow,
    LeftBracket,
    RightBracket,
    Comma,
    LeftBoxBracket,
    RightBoxBracket,
    NonPure,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    And,
    Or,
    Xor,
    Negation,
    Plus,
    Minus,
    Division,
    Multiplication,
    Exponentiation,
    Modulo,
    Eq,
    Lt,
    Gt,
    LEq,
    GEq,
}

pub struct Line {
    spaces: u16,
    tabs: u16,
    tokens: Vec<Token>
}

macro_rules! read_while_alphanumeric_into {
    ($iter:expr, $string:expr) => {
        while let Some(ch) = $iter.peek() {
            if !ch.is_alphanumeric() && *ch != '.' {
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

impl Line {
    pub fn new(line_str: String) -> Self {
        let mut spaces: u16 = 0;
        let mut tabs: u16 = 0;
        let mut iter = line_str.chars().peekable();

        while let Some(ch) = iter.peek() {
            if *ch == ' ' {
                spaces += 1;
                iter.next();
            } else if *ch == '\t' {
                tabs += 1;
                iter.next();
            } else {
                break;
            }
        }

        let mut tokens: Vec<Token> = Vec::new();
        
        while let Some(ch) = iter.next() {
            if ch.is_whitespace() {
                continue;
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
                } else if string == "nonpure" {
                    tokens.push(Token::NonPure);
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
                tokens.push(Token::Operation(Op::Negation));
            } else if ch == '&' {
                tokens.push(Token::Operation(Op::And));
            } else if ch == '|' {
                tokens.push(Token::Operation(Op::Or));
            } else if ch == '^' {
                tokens.push(Token::Operation(Op::Xor));
            } else if ch == '+' {
                tokens.push(Token::Operation(Op::Plus));
            } else if ch == '/' {
                tokens.push(Token::Operation(Op::Division));
            } else if ch == '%' {
                tokens.push(Token::Operation(Op::Modulo));
            }
        }

        Line { spaces, tabs, tokens}
    }
}

pub fn lines<T: Read + 'static>(reader: T) -> Box<dyn Iterator<Item = Result<Line, std::io::Error>>>  {
    let buf_reader = BufReader::new(reader);
    Box::new(buf_reader.lines().map(|result| {result.map(Line::new)}))
}


#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! assert_eq_vec {
        ($vec1:expr, $vec2:expr) => {
            for i in 0..std::cmp::max($vec1.len(), $vec2.len()) {
                assert_eq!($vec1[i], $vec2[i]);
            }
        };
    }

    #[test]
    fn test_all_lexemes_line() {
        let lexemes = "   variable \"\" \"word with space\"   \'\' \'char\' 55.44 1234 0 & | ^ ! + - / * ** % == < > <= >= if else then = -> ( ) , [ ] nonpure";
        let lines = lines(lexemes.as_bytes()).collect::<Vec<_>>();
        assert_eq!(lines.len(), 1);
        let line = lines[0].as_ref().unwrap();

        let expected = vec![
            Token::Name(String::from("variable")), 
            Token::Str(String::from("")), 
            Token::Str(String::from("word with space")), 
            Token::Char(String::from("")),
            Token::Char(String::from("char")), 
            Token::Number(String::from("55.44")), 
            Token::Number(String::from("1234")),
            Token::Number(String::from("0")), 
            Token::Operation(Op::And),
            Token::Operation(Op::Or),
            Token::Operation(Op::Xor),
            Token::Operation(Op::Negation),
            Token::Operation(Op::Plus),
            Token::Operation(Op::Minus),
            Token::Operation(Op::Division),
            Token::Operation(Op::Multiplication),
            Token::Operation(Op::Exponentiation),
            Token::Operation(Op::Modulo),
            Token::Operation(Op::Eq),
            Token::Operation(Op::Lt),
            Token::Operation(Op::Gt),
            Token::Operation(Op::LEq),
            Token::Operation(Op::GEq),
            Token::If,
            Token::Else,
            Token::Then,
            Token::Assignment,
            Token::Arrow,
            Token::LeftBracket,
            Token::RightBracket,
            Token::Comma,
            Token::LeftBoxBracket,
            Token::RightBoxBracket,
            Token::NonPure];

        assert_eq_vec!(line.tokens, expected);
    }

    #[test]
    fn test_indentation() {
        let input = "var\n var\n  var\n   var\n\tvar\n\t var\n\t  \tvar\n\t\tvar\n";
        let lines = lines(input.as_bytes()).collect::<Vec<_>>();
        assert_eq!(lines[0].as_ref().unwrap().spaces, 0);
        assert_eq!(lines[0].as_ref().unwrap().tabs, 0);
        assert_eq!(lines[1].as_ref().unwrap().spaces, 1);
        assert_eq!(lines[1].as_ref().unwrap().tabs, 0);
        assert_eq!(lines[2].as_ref().unwrap().spaces, 2);
        assert_eq!(lines[2].as_ref().unwrap().tabs, 0);
        assert_eq!(lines[3].as_ref().unwrap().spaces, 3);
        assert_eq!(lines[3].as_ref().unwrap().tabs, 0);
        assert_eq!(lines[4].as_ref().unwrap().spaces, 0);
        assert_eq!(lines[4].as_ref().unwrap().tabs, 1);
        assert_eq!(lines[5].as_ref().unwrap().spaces, 1);
        assert_eq!(lines[5].as_ref().unwrap().tabs, 1);
        assert_eq!(lines[6].as_ref().unwrap().spaces, 2);
        assert_eq!(lines[6].as_ref().unwrap().tabs, 2);
        assert_eq!(lines[7].as_ref().unwrap().spaces, 0);
        assert_eq!(lines[7].as_ref().unwrap().tabs, 2);
    }

    #[test]
    fn test_multiple_lines() {
        let input = "  fun = (a, b) ->\n   sum = a + b\n   sum";
        let lines = lines(input.as_bytes()).collect::<Vec<_>>();
        assert_eq!(lines[0].as_ref().unwrap().spaces, 2);
        assert_eq!(lines[0].as_ref().unwrap().tabs, 0);
        assert_eq_vec!(lines[0].as_ref().unwrap().tokens, vec![
            Token::Name(String::from("fun")),
            Token::Assignment,
            Token::LeftBracket,
            Token::Name(String::from("a")),
            Token::Comma,
            Token::Name(String::from("b")),
            Token::RightBracket,
            Token::Arrow,
        ]);
        assert_eq!(lines[1].as_ref().unwrap().spaces, 3);
        assert_eq!(lines[1].as_ref().unwrap().tabs, 0);
        assert_eq_vec!(lines[1].as_ref().unwrap().tokens, vec![
            Token::Name(String::from("sum")),
            Token::Assignment,
            Token::Name(String::from("a")),
            Token::Operation(Op::Plus),
            Token::Name(String::from("b")),
        ]);
        assert_eq!(lines[2].as_ref().unwrap().spaces, 3);
        assert_eq!(lines[2].as_ref().unwrap().tabs, 0);
        assert_eq_vec!(lines[2].as_ref().unwrap().tokens, vec![
            Token::Name(String::from("sum")),
        ]);
    }

    #[test]
    fn test_multiple_lines_no_whitespace() {
        let input = "  fun=(a,b)->\n   prod=a*b\n   prod";
        let lines = lines(input.as_bytes()).collect::<Vec<_>>();
        assert_eq!(lines[0].as_ref().unwrap().spaces, 2);
        assert_eq!(lines[0].as_ref().unwrap().tabs, 0);
        assert_eq_vec!(lines[0].as_ref().unwrap().tokens, vec![
            Token::Name(String::from("fun")),
            Token::Assignment,
            Token::LeftBracket,
            Token::Name(String::from("a")),
            Token::Comma,
            Token::Name(String::from("b")),
            Token::RightBracket,
            Token::Arrow,
        ]);
        assert_eq!(lines[1].as_ref().unwrap().spaces, 3);
        assert_eq!(lines[1].as_ref().unwrap().tabs, 0);
        assert_eq_vec!(lines[1].as_ref().unwrap().tokens, vec![
            Token::Name(String::from("prod")),
            Token::Assignment,
            Token::Name(String::from("a")),
            Token::Operation(Op::Multiplication),
            Token::Name(String::from("b")),
        ]);
        assert_eq!(lines[2].as_ref().unwrap().spaces, 3);
        assert_eq!(lines[2].as_ref().unwrap().tabs, 0);
        assert_eq_vec!(lines[2].as_ref().unwrap().tokens, vec![
            Token::Name(String::from("prod")),
        ]);
    }
}
