mod enums;
use std::iter::Peekable;
use std::slice::Iter;

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
}

fn parse_binary_operation(
    op: &Op,
    expr_1: Expression,
    expr_2: Expression,
) -> Result<Expression, ParserError> {
    todo!();
}

fn parse_string(string: &str) -> Value {
    if string.len() == 0 {
        Value::Nil
    } else {
        Value::Tuple(
            Box::new(Value::Char(string.chars().next().unwrap())),
            Box::new(parse_string(&string[1..string.len()])),
        )
    }
}

fn parse_number(string: &str, line_num: u32) -> Result<Expression, ParserError> {
    if let Some(integer) = string.parse::<i32>().ok() {
        Ok(Expression::Value(Value::Number(Number::Integer(integer))))
    } else if let Some(float) = string.parse::<f64>().ok() {
        Ok(Expression::Value(Value::Number(Number::Float(float))))
    } else {
        Err(ParserError::NumberParseError { line: line_num })
    }
}

fn parse_args_list(tokens: &mut Peekable<Iter<Token>>, line_num: u32) -> Result<Vec<Expression>, ParserError> {
    let mut vec = vec![];
    
    loop {
        let token = tokens.next().ok_or(ParserError::UnexpectedEndOfLine{line: line_num})?;
        match token {
            Token::RightBracket => return Ok(vec),
            _ => {
                let expr = parse_expression(tokens, line_num)?;
                vec.push(expr);
                if let Some(Token::Comma) = tokens.peek() {
                    tokens.next().unwrap();
                    continue;
                } else {
                    return Err(ParserError::CommaExpected { line: line_num });
                }
            }
        };
    }
}

fn parse_expression(
    tokens: &mut Peekable<Iter<Token>>,
    line_num: u32,
) -> Result<Expression, ParserError> {
    let token = tokens
        .next()
        .ok_or(ParserError::ExpressionExpected { line: line_num })?;
    let expr = match token {
        Token::Name(string) => {
            if let Some(Token::LeftBracket) = tokens.peek() {                
                tokens.next().unwrap();
                let expr_vec = parse_args_list(tokens, line_num)?;
                Expression::FunctionCall { args: expr_vec }            
            } else {
                Expression::Name(string.to_string())
            }
        }
        Token::Number(string) => parse_number(string, line_num)?,
        Token::Str(string) => Expression::Value(parse_string(string)),
        Token::Char(string) => {
            if string.len() != 1 {
                return Err(ParserError::CharParseError { line: line_num })
            } else {
                Expression::Value(Value::Char(string.chars().next().unwrap()))
            }
        }
        Token::NonPure => {
            // TODO: call recursively and match if it is a function
        }
        Token::LeftBoxBracket => {
            // TODO: parse function
        }
        Token::If => {

        }
        Token::LeftBracket => {
            let expr = parse_expression(tokens, line_num)?;

            let next_token = tokens
                .next()
                .ok_or(ParserError::UnexpectedEndOfLine { line: line_num })?;
            match next_token {
                Token::RightBracket => (),
                _ => return Err(ParserError::UnbalancedBracketsError { line: line_num }),
            }
            expr
        }
    };

    if let Some(Token::Operation(op)) = tokens.peek() {        
        tokens.next().unwrap();
        let expr_2 = parse_expression(tokens, line_num)?;
        parse_binary_operation(op, expr, expr_2)
    } else {
        Ok(expr)
    }
}

enum FunctionLine {
    Expression(Expression),
    Assignment(String, Expression),
    Empty,
}

fn parse_line(line: &Line) -> Result<FunctionLine, ParserError> {
    let tokens = &line.tokens;

    if tokens.len() == 0 {
        return Ok(FunctionLine::Empty);
    }

    if tokens.len() > 2 && matches!(tokens[1], Token::Assignment) {
        match &tokens[0] {
            Token::Name(string) => {
                let mut iter = tokens[2..].iter().peekable();
                let expression = parse_expression(&mut iter, line.number)?;
                Ok(FunctionLine::Assignment(string.clone(), expression))
            }
            _ => Err(ParserError::AssignmentError {
                line: line.number,
                msg: String::from("The token on the left side of the assignment is not a name!"),
            }),
        }
    } else {
        let mut iter = tokens.iter().peekable();
        parse_expression(&mut iter, line.number).map(|exp| FunctionLine::Expression(exp))
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
                    msg: String::from(
                        "Cannot have assignments after expression in a single scope!",
                    ),
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

pub fn parse(lines: LinesIterator) -> Result<Scope, ParserError> {
    let mut lines = lines.peekable();
    let function_body = parse_scope(&mut lines, 0)?;

    Ok(Scope {
        assignments: function_body.assignments,
        expression: function_body.expression,
    })
}
