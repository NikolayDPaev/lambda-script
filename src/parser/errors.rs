use crate::lexer::enums::Token;

#[derive(Debug, PartialEq)]
pub enum ParserError {
    PeekError,
    LexerError,
    IndentationError {
        line: u32,
        msg: String,
        expected: i32,
        actual: i32,
    },
    UnexpectedExpressionInTopLevelOfImport {
        line: u32,
    },
    CannotImportFile {
        line: u32,
        filename: String,
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
    FilenameStringExpected {
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

pub fn process_parser_error(err: ParserError) -> String {
    match err {
        ParserError::PeekError => format!("Error peeking next token"),
        ParserError::LexerError => format!("Error in the underlying lexer"),
        ParserError::IndentationError { line, msg, expected, actual } => format!("Error on line {:?}, {:?}, expected indentation of {:?}, actual: {:?}", line, msg, expected, actual),
        ParserError::UnexpectedExpressionInTopLevelOfImport { line } => format!("Error on line {:?}: Imports must not have expression", line),
        ParserError::CannotImportFile { line, filename } => format!("Error on line {:?}: Invalid import of file {:?}", line, filename),
        ParserError::ExpressionExpected { line } => format!("Error on line {:?}: expected expression", line),
        ParserError::CommaExpected { line } => format!("Error on line {:?}: expected comma", line),
        ParserError::UnexpectedEOF => format!("Error unexpected end of file"),
        ParserError::UnexpectedEndOfLine { line } => format!("Unexpected end of file: {:?}", line),
        ParserError::ReturnExpressionError { line, msg } => format!("Error on line {:?}: {:?}", line, msg),
        ParserError::AssignmentError { line, msg } => format!("Error on line {:?}: {:?}", line, msg),
        ParserError::UnbalancedBracketsError { line } => format!("Unbalanced brackets on line: {:?}", line),
        ParserError::NumberParseError { line } => format!("Error parsing number on line: {:?}", line),
        ParserError::CharParseError { line } => format!("Error parsing char on line: {:?}", line),
        ParserError::FilenameStringExpected { line } => format!("Expected filename string on line: {:?}", line),
        ParserError::NameExpected { line } => format!("Error on line {:?}: expected name", line),
        ParserError::ArrowExpected { line } => format!("Error on line {:?}: expected arrow", line),
        ParserError::LeftBracketExpected { line } => format!("Error on line {:?}: expected opening bracket", line),
        ParserError::ThenExpected { line } => format!("Error on line {:?}: expected token then", line),
        ParserError::ElseExpected { line } => format!("Error on line {:?}: expected token else", line),
        ParserError::UnexpectedToken { line, token } => format!("Error on line {:?}: unexpected token {:?}", line, token),
        ParserError::OpError { line } => format!("Error on line {:?}: unexpected operation", line),
    }
}
