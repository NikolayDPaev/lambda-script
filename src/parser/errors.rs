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
            ParserErrorKind::PeekError => format!("error peeking next token"),
            ParserErrorKind::LexerError => format!("error in the underlying lexer"),
            ParserErrorKind::IndentationError {
                msg,
                expected,
                actual,
            } => format!(
                "{:?}, expected indentation of {:?}, actual: {:?}",
                msg, expected, actual
            ),
            ParserErrorKind::UnknownNameError { name } => format!("Unknown name: {:?}", name),
            ParserErrorKind::CannotImportFile {
                import_filename,
                error_message,
            } => {
                format!(
                    "invalid import of file {:?}, error: {}",
                    import_filename, error_message
                )
            }
            ParserErrorKind::ExpressionExpected => {
                format!("expected expression")
            }
            ParserErrorKind::CommaExpected => format!("expected comma"),
            ParserErrorKind::UnexpectedEOF => format!("unexpected end of file"),
            ParserErrorKind::UnexpectedEndOfLine => format!("unexpected end of file"),
            ParserErrorKind::ReturnExpressionError { msg } => {
                format!("{:?}", msg)
            }
            ParserErrorKind::AssignmentError { msg } => {
                format!("{:?}", msg)
            }
            ParserErrorKind::UnbalancedBracketsError => {
                format!("unbalanced brackets")
            }
            ParserErrorKind::NumberParseError => {
                format!("error parsing number")
            }
            ParserErrorKind::CharParseError => format!("error parsing char"),
            ParserErrorKind::FilenameStringExpected => {
                format!("expected filename string ")
            }
            ParserErrorKind::NameExpected => format!("expected name"),
            ParserErrorKind::ArrowExpected => format!("expected arrow"),
            ParserErrorKind::LeftBracketExpected => {
                format!("expected opening bracket")
            }
            ParserErrorKind::ThenExpected => {
                format!("expected token then")
            }
            ParserErrorKind::ElseExpected => {
                format!("expected token else")
            }
            ParserErrorKind::UnexpectedToken { token } => {
                format!("unexpected token {:?}", token)
            }
            ParserErrorKind::OpError => format!("unexpected operation"),
        };

        format!(
            "Error parsing file: {}, line: {}:\nError: {}.\n",
            self.filename, self.line, message
        )
    }
}
