use std::io::{self, BufRead, BufReader, Read};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Name(String),
    Str(String),
    Char(String),
    Number(String),
    Operation(Op),
    True,
    False,
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
    Cons,
    Left,
    Right,
    Empty,
    Nil,
    Read,
    Print,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Op {
    And,
    Or,
    Xor,
    Negation,
    Plus,
    Minus,
    Division,
    IntDivision,
    Multiplication,
    Exponentiation,
    Modulo,
    NEq,
    Eq,
    Lt,
    Gt,
    LEq,
    GEq,
}

#[derive(Debug)]
pub struct Line {
    pub number: u32,
    pub indentation: u16,
    pub tokens: Vec<Token>,
}

macro_rules! read_while_alphanumeric_into {
    ($iter:expr, $string:expr) => {
        while let Some(ch) = $iter.peek() {
            if !ch.is_alphanumeric() && *ch != '.' && *ch != '_' && *ch != '-' {
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
                } else if string == "nonpure" {
                    tokens.push(Token::NonPure);
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
            }else if ch == '/' {
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
            .zip(std::ops::RangeFrom { start: 1 })
            .map(|(result, line_num)| result.map(|line| Line::new(line, line_num as u32))),
    )
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
        let lexemes = "   variable \"\" \"word with space\"   \'\' \'char\' 55.44 1234 0 & | ^ ! + - / // * ** % == != < > <= >= true false if else then = -> ( ) , [ ] nonpure cons left right empty nil read print";
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
            Token::Operation(Op::IntDivision),
            Token::Operation(Op::Multiplication),
            Token::Operation(Op::Exponentiation),
            Token::Operation(Op::Modulo),
            Token::Operation(Op::Eq),
            Token::Operation(Op::NEq),
            Token::Operation(Op::Lt),
            Token::Operation(Op::Gt),
            Token::Operation(Op::LEq),
            Token::Operation(Op::GEq),
            Token::True,
            Token::False,
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
            Token::NonPure,
            Token::Cons,
            Token::Left,
            Token::Right,
            Token::Empty,
            Token::Nil,
            Token::Read,
            Token::Print,
        ];

        assert_eq_vec!(line.tokens, expected);
    }

    #[test]
    fn test_indentation() {
        let input = "var\n var\n  var\n   var\n\tvar\n\t var\n\t  \tvar\n\t\tvar\n";
        let lines = lines(input.as_bytes()).collect::<Vec<_>>();
        assert_eq!(lines[0].as_ref().unwrap().indentation, 0);
        assert_eq!(lines[0].as_ref().unwrap().number, 1);
        assert_eq!(lines[1].as_ref().unwrap().indentation, 1);
        assert_eq!(lines[1].as_ref().unwrap().number, 2);
        assert_eq!(lines[2].as_ref().unwrap().indentation, 2);
        assert_eq!(lines[2].as_ref().unwrap().number, 3);
        assert_eq!(lines[3].as_ref().unwrap().indentation, 3);
        assert_eq!(lines[3].as_ref().unwrap().number, 4);
        assert_eq!(lines[4].as_ref().unwrap().indentation, 4);
        assert_eq!(lines[4].as_ref().unwrap().number, 5);
        assert_eq!(lines[5].as_ref().unwrap().indentation, 5);
        assert_eq!(lines[5].as_ref().unwrap().number, 6);
        assert_eq!(lines[6].as_ref().unwrap().indentation, 10);
        assert_eq!(lines[6].as_ref().unwrap().number, 7);
        assert_eq!(lines[7].as_ref().unwrap().indentation, 8);
        assert_eq!(lines[7].as_ref().unwrap().number, 8);
        
    }

    #[test]
    fn test_multiple_lines() {
        let input = "  fun = (a, b) ->\n   sum = a + b\n   sum";
        let lines = lines(input.as_bytes()).collect::<Vec<_>>();
        assert_eq!(lines[0].as_ref().unwrap().indentation, 2);
        assert_eq!(lines[0].as_ref().unwrap().number, 1);
        assert_eq_vec!(
            lines[0].as_ref().unwrap().tokens,
            vec![
                Token::Name(String::from("fun")),
                Token::Assignment,
                Token::LeftBracket,
                Token::Name(String::from("a")),
                Token::Comma,
                Token::Name(String::from("b")),
                Token::RightBracket,
                Token::Arrow,
            ]
        );
        assert_eq!(lines[1].as_ref().unwrap().indentation, 3);
        assert_eq!(lines[1].as_ref().unwrap().number, 2);
        assert_eq_vec!(
            lines[1].as_ref().unwrap().tokens,
            vec![
                Token::Name(String::from("sum")),
                Token::Assignment,
                Token::Name(String::from("a")),
                Token::Operation(Op::Plus),
                Token::Name(String::from("b")),
            ]
        );
        assert_eq!(lines[2].as_ref().unwrap().indentation, 3);
        assert_eq!(lines[2].as_ref().unwrap().number, 3);
        assert_eq_vec!(
            lines[2].as_ref().unwrap().tokens,
            vec![Token::Name(String::from("sum")),]
        );
    }

    #[test]
    fn test_multiple_lines_no_whitespace() {
        let input = "  fun=(a,b)->\n   prod=a*b\n   prod";
        let lines = lines(input.as_bytes()).collect::<Vec<_>>();
        assert_eq!(lines[0].as_ref().unwrap().indentation, 2);
        assert_eq!(lines[0].as_ref().unwrap().number, 1);
        assert_eq_vec!(
            lines[0].as_ref().unwrap().tokens,
            vec![
                Token::Name(String::from("fun")),
                Token::Assignment,
                Token::LeftBracket,
                Token::Name(String::from("a")),
                Token::Comma,
                Token::Name(String::from("b")),
                Token::RightBracket,
                Token::Arrow,
            ]
        );
        assert_eq!(lines[1].as_ref().unwrap().indentation, 3);
        assert_eq!(lines[1].as_ref().unwrap().number, 2);
        assert_eq_vec!(
            lines[1].as_ref().unwrap().tokens,
            vec![
                Token::Name(String::from("prod")),
                Token::Assignment,
                Token::Name(String::from("a")),
                Token::Operation(Op::Multiplication),
                Token::Name(String::from("b")),
            ]
        );
        assert_eq!(lines[2].as_ref().unwrap().indentation, 3);
        assert_eq!(lines[2].as_ref().unwrap().number, 3);
        assert_eq_vec!(
            lines[2].as_ref().unwrap().tokens,
            vec![Token::Name(String::from("prod")),]
        );
    }

    #[test]
    fn test_cons() {
        let input = "  list=cons(1, cons(2, cons(3, nil)))";
        let lines = lines(input.as_bytes()).collect::<Vec<_>>();
        assert_eq!(lines[0].as_ref().unwrap().indentation, 2);
        assert_eq!(lines[0].as_ref().unwrap().number, 1);
        assert_eq_vec!(
            lines[0].as_ref().unwrap().tokens,
            vec![
                Token::Name(String::from("list")),
                Token::Assignment,
                Token::Cons,
                Token::LeftBracket,
                Token::Number(String::from("1")),
                Token::Comma,
                Token::Cons,
                Token::LeftBracket,
                Token::Number(String::from("2")),
                Token::Comma,
                Token::Cons,
                Token::LeftBracket,
                Token::Number(String::from("3")),
                Token::Comma,
                Token::Nil,
                Token::RightBracket,
                Token::RightBracket,
                Token::RightBracket,
            ]
        );
    }
}
