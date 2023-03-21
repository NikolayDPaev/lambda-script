use std::fmt;

use crate::lexer::{enums::Token, Line};

#[derive(Debug, PartialEq)]
pub enum ParserErrorKind {
    PeekError,
    LexerError,
    UnknownNameError {
        name: String,
    },
    IndentationError {
        msg: String,
        expected: i32,
        actual: i32,
    },
    CannotImportFile {
        import_filename: String,
        error_message: String,
    },
    ExpressionExpected,
    CommaExpected,
    UnexpectedEOF,
    UnexpectedEndOfLine,
    ReturnExpressionError {
        msg: String,
    },
    AssignmentError {
        msg: String,
    },
    ClosingBracketExpected,
    NumberParseError,
    CharParseError,
    FilenameStringExpected,
    NameExpected,
    ArrowExpected,
    LeftBracketExpected,
    ThenExpected,
    ElseExpected,
    UnexpectedToken {
        token: Token,
    },
    OpError,
}

#[derive(Debug, PartialEq)]
pub struct ParserError {
    pub kind: ParserErrorKind,
    pub filename: String,
    pub line: Option<Line>,
    pub token_pos: usize,
}

fn show_error_token(indentation: u16, tokens: &[Token], token_index: usize) -> String {
    let mut error_token_index: usize = indentation.into();
    let mut line = std::iter::repeat(" ")
        .take(indentation.into())
        .collect::<String>();

    for i in 0..tokens.len() {
        let token_string = format!("{}", tokens[i]);
        if i < token_index {
            error_token_index += token_string.len();
        }
        line.push_str(&token_string);
    }

    format!(
        "{}\n{arrow: >pos$}",
        line,
        arrow = '^',
        pos = error_token_index + 1
    )
}

impl fmt::Display for ParserError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let message = match &self.kind {
            ParserErrorKind::PeekError => format!("Error peeking next token"),
            ParserErrorKind::LexerError => format!("Error in the underlying lexer"),
            ParserErrorKind::IndentationError {
                msg,
                expected,
                actual,
            } => format!("{}, expected: {}, actual: {}", msg, expected, actual),
            ParserErrorKind::UnknownNameError { name } => format!("Unknown name: {}", name),
            ParserErrorKind::CannotImportFile {
                import_filename,
                error_message,
            } => {
                format!(
                    "Invalid import of file {}, error: {}",
                    import_filename, error_message
                )
            }
            ParserErrorKind::ExpressionExpected => {
                format!("Expected expression")
            }
            ParserErrorKind::CommaExpected => format!("Expected comma"),
            ParserErrorKind::UnexpectedEOF => format!("Unexpected end of file"),
            ParserErrorKind::UnexpectedEndOfLine => format!("Unexpected end of file"),
            ParserErrorKind::ReturnExpressionError { msg } => {
                format!("{}", msg)
            }
            ParserErrorKind::AssignmentError { msg } => {
                format!("{}", msg)
            }
            ParserErrorKind::ClosingBracketExpected => {
                format!("Closing bracket expected")
            }
            ParserErrorKind::NumberParseError => {
                format!("Error parsing number")
            }
            ParserErrorKind::CharParseError => format!("Error parsing char"),
            ParserErrorKind::FilenameStringExpected => {
                format!("Expected single filename string ")
            }
            ParserErrorKind::NameExpected => format!("Expected name"),
            ParserErrorKind::ArrowExpected => format!("Expected arrow"),
            ParserErrorKind::LeftBracketExpected => {
                format!("Expected opening bracket")
            }
            ParserErrorKind::ThenExpected => {
                format!("Expected token then")
            }
            ParserErrorKind::ElseExpected => {
                format!("Expected token else")
            }
            ParserErrorKind::UnexpectedToken { token } => {
                format!("Unexpected token {:?}", token)
            }
            ParserErrorKind::OpError => format!("Unexpected operation"),
        };

        match &self.line {
            Some(line) => write!(
                fmt,
                "Error parsing file: {}\nline {}:\n\n{} {}\n",
                self.filename,
                line.number,
                show_error_token(line.indentation, &line.tokens, self.token_pos),
                message
            ),
            None => write!(fmt, "Error parsing file: {}\n{}\n", self.filename, message),
        }
    }
}
