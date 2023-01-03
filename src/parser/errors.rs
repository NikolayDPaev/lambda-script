use crate::lexer::Token;

#[derive(Debug, PartialEq)]
pub enum ParserError {
    PeekError,
    LexerError,
    IndentationError {
        line: u32,
        msg: String,
        expected: u16,
        actual: u16,
    },
    ExpressionExpected {
        line: u32,
    },
    CommaExpected {
        line: u32,
    },
    UnexpectedEOF,
    UnexpectedEndOfLine {
        line: u32,
    },
    ReturnExpressionError {
        line: u32,
        msg: String,
    },
    AssignmentError {
        line: u32,
        msg: String,
    },
    UnbalancedBracketsError {
        line: u32,
    },
    NumberParseError {
        line: u32,
    },
    CharParseError {
        line: u32,
    },
    FunctionExpected {
        line: u32,
    },
    NameExpected {
        line: u32,
    },
    ArrowExpected {
        line: u32,
    },
    LeftBracketExpected {
        line: u32,
    },
    ThenExpected {
        line: u32,
    },
    ElseExpected {
        line: u32,
    },
    UnexpectedToken {
        line: u32,
        token: Token,
    },
    OpError {
        line: u32,
    }
}
