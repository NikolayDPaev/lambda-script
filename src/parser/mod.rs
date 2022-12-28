mod enums;
use std::iter::Peekable;

use crate::lexer::*;
use crate::parser::enums::*;

pub enum ParserError {
    PeekError,
    LexerError(std::io::Error),
    IndentationError {
        line: u32,
        msg: String,
        expected: u16,
        actual: u16,
    },
    ExpressionExpected {
        line: u32,
    },
    UnexpectedEOF,
    ReturnExpressionError {
        line: u32,
        msg: String,
    },
    AssignmentError {
        line: u32,
        msg: String,
    }
}

pub struct Scope {
    assignments: Vec<(String, Expression)>,
    expression: Expression,
}

enum FunctionLine {
    Expression(Expression),
    Assignment(String, Expression),
    Empty,
}

fn parse_expression(tokens: &[Token]) -> Result<Expression, ParserError> {
    todo!()
}

fn parse_line(line: &Line) -> Result<FunctionLine, ParserError> {
    let tokens = &line.tokens;

    if tokens.len() == 0 { 
        return Ok(FunctionLine::Empty);
    }

    if tokens.len() > 2 && matches!(tokens[1], Token::Assignment) {
        match &tokens[0] {
            Token::Name(string) => {
                let expression = parse_expression(&tokens[2..])?;
                Ok(FunctionLine::Assignment(string.clone(), expression))
            },
            _ => Err(ParserError::AssignmentError { line: line.number, msg: String::from("The token on the left side of the assignment is not a name!") })
        }
    } else {
        parse_expression(tokens.as_slice()).map(|exp| FunctionLine::Expression(exp))
    }
}

fn handle_line(
    line: &Line,
    expression: &mut Option<Expression>,
    assignments: &mut Vec<(String, Expression)>,
) -> Result<(), ParserError> {
    match parse_line(line)? {
        FunctionLine::Expression(exp) => match expression {
            Some(_) => {
                return Err(ParserError::ReturnExpressionError {
                    line: line.number,
                    msg: String::from("A scope can only have 1 expression!"),
                })
            }
            None => *expression = Some(exp),
        },
        FunctionLine::Assignment(string, exp) => match expression {
            Some(_) => {
                return Err(ParserError::ReturnExpressionError {
                    line: line.number,
                    msg: String::from("Cannot have assignments after expression in a single scope!"),
                })
            }
            None => assignments.push((string, exp)),
        },
        FunctionLine::Empty => (),
    };
    Ok(())
}

fn parse_scope(
    lines: &mut Peekable<LinesIterator>,
    outside_indentation: u16,
) -> Result<Scope, ParserError> {
    let mut assignments = Vec::<(String, Expression)>::new();
    let mut expression: Option<enums::Expression> = None;

    let line = lines
        .next()
        .ok_or(ParserError::UnexpectedEOF)?
        .map_err(|err| ParserError::LexerError(err))?;

    let scope_indentation = line.indentation;
    if scope_indentation <= outside_indentation {
        return Err(ParserError::IndentationError {
            line: line.number,
            msg: String::from("Scope has to be indented"),
            expected: outside_indentation + 1,
            actual: outside_indentation,
        });
    }

    handle_line(&line, &mut expression, &mut assignments)?;

    while let Some(line_result) = lines.peek() {
        let next_line = line_result.as_ref().map_err(|_| ParserError::PeekError)?;
        if next_line.indentation == outside_indentation {
            // end of scope
            break;
        } else if next_line.indentation != scope_indentation {
            // scope has not ended yet but has different indentation
            return Err(ParserError::IndentationError {
                line: line.number,
                msg: String::from("The indentation of a function should be the same"),
                expected: scope_indentation,
                actual: line.indentation,
            });
        } else {
            // scope has not ended and has valid indentation
            let line = lines
                .next()
                .unwrap()
                .map_err(|err| ParserError::LexerError(err))?;

            handle_line(&line, &mut expression, &mut assignments)?
        }
    }
    
    match expression {
        Some(expression) => Ok(Scope {
            assignments,
            expression,
        }),
        None => Err(ParserError::ExpressionExpected { line: line.number }),
    }
}

pub fn parse(mut lines: LinesIterator) -> Result<Scope, ParserError> {
    let mut lines = lines.peekable();
    let function_body = parse_scope(&mut lines, 0)?;

    Ok(Scope {
        assignments: function_body.assignments,
        expression: function_body.expression,
    })
}
