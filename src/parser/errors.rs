use crate::lexer::enums::Token;

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
    UnbalancedBracketsError,
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
    pub line: u32,
}

impl ParserError {
    pub fn get_message(self) -> String {
        let message = match self.kind {
            ParserErrorKind::PeekError => format!("Error peeking next token"),
            ParserErrorKind::LexerError => format!("Error in the underlying lexer"),
            ParserErrorKind::IndentationError {
                msg,
                expected,
                actual,
            } => format!(
                "{}, expected: {}, actual: {}",
                msg, expected, actual
            ),
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
            ParserErrorKind::UnbalancedBracketsError => {
                format!("Unbalanced brackets")
            }
            ParserErrorKind::NumberParseError => {
                format!("Error parsing number")
            }
            ParserErrorKind::CharParseError => format!("Error parsing char"),
            ParserErrorKind::FilenameStringExpected => {
                format!("Expected filename string ")
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

        format!(
            "Error parsing file: {}\nline {}:\n\t{}\n",
            self.filename, self.line, message
        )
    }
}
