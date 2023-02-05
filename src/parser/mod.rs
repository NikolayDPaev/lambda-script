pub mod enums;
pub mod errors;
#[cfg(test)]
mod tests;
use std::fs::File;
use std::iter::Peekable;
use std::path::PathBuf;
use std::rc::Rc;
use std::slice::Iter;
use std::vec;

use crate::lexer::enums::*;
use crate::lexer::*;
use crate::parser::enums::*;
use crate::parser::errors::*;

macro_rules! new_binary_operation {
    ($op:expr, $expr_1:expr, $expr_2:expr) => {
        Expression::BinaryOperation($op, Rc::new($expr_1), Rc::new($expr_2))
    };
}

fn new_scope_with_single_expr(expr: Expression, pure: bool) -> Box<Scope> {
    if pure {
        Box::new(Scope::Pure {
            assignments: vec![],
            expression: Rc::new(expr),
        })
    } else {
        Box::new(Scope::Impure {
            assignments: vec![],
            statements: vec![Rc::new(expr)],
        })
    }
}

fn parse_binary_operation(
    op: &Op,
    expr_1: Expression,
    expr_2: Expression,
    line_num: u32,
    filename: &str,
) -> Result<Expression, ParserError> {
    Ok(match op {
        Op::And => new_binary_operation!(BinaryOp::Boolean(BoolBinOp::And), expr_1, expr_2),
        Op::Or => new_binary_operation!(BinaryOp::Boolean(BoolBinOp::Or), expr_1, expr_2),
        Op::Xor => new_binary_operation!(BinaryOp::Boolean(BoolBinOp::Xor), expr_1, expr_2),
        Op::Plus => new_binary_operation!(BinaryOp::Arithmetic(ArithBinOp::Plus), expr_1, expr_2),
        Op::Minus => new_binary_operation!(BinaryOp::Arithmetic(ArithBinOp::Minus), expr_1, expr_2),
        Op::Division => {
            new_binary_operation!(BinaryOp::Arithmetic(ArithBinOp::Division), expr_1, expr_2)
        }
        Op::IntDivision => {
            new_binary_operation!(
                BinaryOp::Arithmetic(ArithBinOp::IntDivision),
                expr_1,
                expr_2
            )
        }
        Op::Multiplication => new_binary_operation!(
            BinaryOp::Arithmetic(ArithBinOp::Multiplication),
            expr_1,
            expr_2
        ),
        Op::Exponentiation => new_binary_operation!(
            BinaryOp::Arithmetic(ArithBinOp::Exponentiation),
            expr_1,
            expr_2
        ),
        Op::Modulo => {
            new_binary_operation!(BinaryOp::Arithmetic(ArithBinOp::Modulo), expr_1, expr_2)
        }
        Op::Eq => new_binary_operation!(BinaryOp::Compare(CmpBinOp::Eq), expr_1, expr_2),
        Op::NEq => new_binary_operation!(BinaryOp::Compare(CmpBinOp::NEq), expr_1, expr_2),
        Op::Lt => new_binary_operation!(BinaryOp::Compare(CmpBinOp::Lt), expr_1, expr_2),
        Op::Gt => new_binary_operation!(BinaryOp::Compare(CmpBinOp::Gt), expr_1, expr_2),
        Op::LEq => new_binary_operation!(BinaryOp::Compare(CmpBinOp::LEq), expr_1, expr_2),
        Op::GEq => new_binary_operation!(BinaryOp::Compare(CmpBinOp::GEq), expr_1, expr_2),
        Op::Negation => {
            return Err(ParserError {
                kind: ParserErrorKind::OpError,
                line: line_num,
                filename: filename.to_string(),
            })
        }
    })
}

pub fn parse_string(string: &str) -> Value {
    string.chars().rev().fold(Value::Nil, |acc, x| {
        Value::Tuple(Box::new(Value::Char(x)), Box::new(acc))
    })
}

fn parse_number(string: &str, line_num: u32, filename: &str) -> Result<Expression, ParserError> {
    if let Some(integer) = string.parse::<i32>().ok() {
        Ok(Expression::Value(Value::Number(Number::Integer(integer))))
    } else if let Some(float) = string.parse::<f64>().ok() {
        Ok(Expression::Value(Value::Number(Number::Float(float))))
    } else {
        Err(ParserError {
            kind: ParserErrorKind::NumberParseError,
            filename: filename.to_string(),
            line: line_num,
        })
    }
}

fn parse_args_list(
    tokens: &mut Peekable<Iter<Token>>,
    line_num: u32,
    next_lines: &mut Peekable<LinesIterator>,
    indentation: u16,
    pure: bool,
    filename: &str,
) -> Result<Vec<Rc<Expression>>, ParserError> {
    let mut vec = vec![];

    loop {
        let token = tokens.peek().ok_or(ParserError {
            kind: ParserErrorKind::UnexpectedEndOfLine,
            filename: filename.to_string(),
            line: line_num,
        })?;

        match token {
            Token::RightBracket => {
                tokens.next().unwrap();
                return Ok(vec);
            }
            _ => {
                let expr =
                    parse_expression(tokens, line_num, next_lines, indentation, pure, filename)?;
                vec.push(Rc::new(expr));
                match tokens.peek() {
                    Some(Token::Comma) => {
                        tokens.next().unwrap();
                    }
                    Some(Token::RightBracket) => (),
                    _ => {
                        return Err(ParserError {
                            kind: ParserErrorKind::CommaExpected,
                            filename: filename.to_string(),
                            line: line_num,
                        })
                    }
                };
            }
        };
    }
}

fn parse_param_list(
    tokens: &mut Peekable<Iter<Token>>,
    line_num: u32,
    filename: &str,
) -> Result<Vec<String>, ParserError> {
    let mut vec = vec![];

    loop {
        let token = tokens.next().ok_or(ParserError {
            kind: ParserErrorKind::UnexpectedEndOfLine,
            filename: filename.to_string(),
            line: line_num,
        })?;
        match token {
            Token::RightBoxBracket => return Ok(vec),
            Token::Name(string) => {
                vec.push(string.clone());
                match tokens.peek() {
                    Some(Token::Comma) => {
                        tokens.next().unwrap();
                    }
                    Some(Token::RightBoxBracket) => (),
                    _ => {
                        return Err(ParserError {
                            kind: ParserErrorKind::CommaExpected,
                            filename: filename.to_string(),
                            line: line_num,
                        })
                    }
                };
            }
            _ => {
                return Err(ParserError {
                    kind: ParserErrorKind::NameExpected,
                    filename: filename.to_string(),
                    line: line_num,
                })
            }
        };
    }
}

macro_rules! assert_next_token {
    ($tokens:expr, $token:pat, $error:expr) => {
        match $tokens.next() {
            Some($token) => (),
            _ => return Err($error),
        };
    };
}

fn parse_expression(
    tokens: &mut Peekable<Iter<Token>>,
    line_num: u32,
    next_lines: &mut Peekable<LinesIterator>,
    indentation: u16,
    pure: bool,
    filename: &str,
) -> Result<Expression, ParserError> {
    let token = tokens.next().ok_or(ParserError {
        kind: ParserErrorKind::ExpressionExpected,
        filename: filename.to_string(),
        line: line_num,
    })?;
    let expr = match token {
        Token::Name(string) => {
            if let Some(Token::LeftBracket) = tokens.peek() {
                tokens.next().unwrap();
                let expr_vec =
                    parse_args_list(tokens, line_num, next_lines, indentation, pure, filename)?;
                Expression::FunctionCall {
                    name: Rc::new(Expression::Name(string.clone())),
                    args: expr_vec,
                }
            } else {
                Expression::Name(string.clone())
            }
        }
        Token::Number(string) => parse_number(string, line_num, filename)?,
        Token::Str(string) => Expression::Value(parse_string(string)),
        Token::Char(string) => {
            if string.chars().count() != 1 {
                return Err(ParserError {
                    kind: ParserErrorKind::CharParseError,
                    filename: filename.to_string(),
                    line: line_num,
                });
            } else {
                Expression::Value(Value::Char(string.chars().next().unwrap()))
            }
        }
        Token::True => Expression::Value(Value::Boolean(true)),
        Token::False => Expression::Value(Value::Boolean(false)),
        Token::Operation(Op::Minus) => {
            let expr = parse_expression(tokens, line_num, next_lines, indentation, pure, filename)?;
            Expression::UnaryOperation(UnaryOp::Minus, Rc::new(expr))
        }
        Token::Operation(Op::Negation) => {
            let expr = parse_expression(tokens, line_num, next_lines, indentation, pure, filename)?;
            Expression::UnaryOperation(UnaryOp::Negation, Rc::new(expr))
        }
        Token::Nil => Expression::Value(Value::Nil),
        Token::Cons => {
            assert_next_token!(
                tokens,
                Token::LeftBracket,
                ParserError {
                    kind: ParserErrorKind::LeftBracketExpected,
                    filename: filename.to_string(),
                    line: line_num
                }
            );
            let expr_1 = Rc::new(parse_expression(
                tokens,
                line_num,
                next_lines,
                indentation,
                pure,
                filename,
            )?);
            assert_next_token!(
                tokens,
                Token::Comma,
                ParserError {
                    kind: ParserErrorKind::CommaExpected,
                    filename: filename.to_string(),
                    line: line_num
                }
            );
            let expr_2 = Rc::new(parse_expression(
                tokens,
                line_num,
                next_lines,
                indentation,
                pure,
                filename,
            )?);
            assert_next_token!(
                tokens,
                Token::RightBracket,
                ParserError {
                    kind: ParserErrorKind::UnbalancedBracketsError,
                    filename: filename.to_string(),
                    line: line_num
                }
            );
            Expression::Cons(expr_1, expr_2)
        }
        Token::Left => {
            assert_next_token!(
                tokens,
                Token::LeftBracket,
                ParserError {
                    kind: ParserErrorKind::LeftBracketExpected,
                    filename: filename.to_string(),
                    line: line_num
                }
            );
            let expr = Rc::new(parse_expression(
                tokens,
                line_num,
                next_lines,
                indentation,
                pure,
                filename,
            )?);
            assert_next_token!(
                tokens,
                Token::RightBracket,
                ParserError {
                    kind: ParserErrorKind::UnbalancedBracketsError,
                    filename: filename.to_string(),
                    line: line_num
                }
            );
            Expression::Left(expr)
        }
        Token::Right => {
            assert_next_token!(
                tokens,
                Token::LeftBracket,
                ParserError {
                    kind: ParserErrorKind::LeftBracketExpected,
                    filename: filename.to_string(),
                    line: line_num
                }
            );
            let expr = Rc::new(parse_expression(
                tokens,
                line_num,
                next_lines,
                indentation,
                pure,
                filename,
            )?);
            assert_next_token!(
                tokens,
                Token::RightBracket,
                ParserError {
                    kind: ParserErrorKind::UnbalancedBracketsError,
                    filename: filename.to_string(),
                    line: line_num
                }
            );
            Expression::Right(expr)
        }
        Token::Empty => {
            assert_next_token!(
                tokens,
                Token::LeftBracket,
                ParserError {
                    kind: ParserErrorKind::LeftBracketExpected,
                    filename: filename.to_string(),
                    line: line_num
                }
            );
            let expr = Rc::new(parse_expression(
                tokens,
                line_num,
                next_lines,
                indentation,
                pure,
                filename,
            )?);
            assert_next_token!(
                tokens,
                Token::RightBracket,
                ParserError {
                    kind: ParserErrorKind::UnbalancedBracketsError,
                    filename: filename.to_string(),
                    line: line_num
                }
            );
            Expression::Empty(expr)
        }
        Token::Print => {
            assert_next_token!(
                tokens,
                Token::LeftBracket,
                ParserError {
                    kind: ParserErrorKind::LeftBracketExpected,
                    filename: filename.to_string(),
                    line: line_num
                }
            );
            let expr = Rc::new(parse_expression(
                tokens,
                line_num,
                next_lines,
                indentation,
                pure,
                filename,
            )?);
            assert_next_token!(
                tokens,
                Token::RightBracket,
                ParserError {
                    kind: ParserErrorKind::UnbalancedBracketsError,
                    filename: filename.to_string(),
                    line: line_num
                }
            );
            Expression::PrintCall(expr)
        }
        Token::Read => Expression::ReadCall,
        Token::Impure => {
            let mut params = Vec::new();
            if let Some(Token::LeftBoxBracket) = tokens.peek() {
                tokens.next().unwrap();
                params = parse_param_list(tokens, line_num, filename)?;
            }
            assert_next_token!(
                tokens,
                Token::Arrow,
                ParserError {
                    kind: ParserErrorKind::ArrowExpected,
                    filename: filename.to_string(),
                    line: line_num
                }
            );

            let scope;
            if let Some(_) = tokens.peek() {
                let expr =
                    parse_expression(tokens, line_num, next_lines, indentation, false, filename)?;
                scope = new_scope_with_single_expr(expr, false);
            } else {
                scope = Box::new(parse_scope(
                    next_lines,
                    indentation.into(),
                    false,
                    filename,
                )?);
            }

            Expression::Value(Value::Function { params, scope })
        }
        Token::Arrow => {
            let scope;
            if let Some(_) = tokens.peek() {
                let expr =
                    parse_expression(tokens, line_num, next_lines, indentation, pure, filename)?;
                scope = new_scope_with_single_expr(expr, true);
            } else {
                scope = Box::new(parse_scope(next_lines, indentation.into(), true, filename)?);
            }
            Expression::Value(Value::Function {
                params: vec![],
                scope,
            })
        }
        Token::LeftBoxBracket => {
            let params = parse_param_list(tokens, line_num, filename)?;
            assert_next_token!(
                tokens,
                Token::Arrow,
                ParserError {
                    kind: ParserErrorKind::ArrowExpected,
                    filename: filename.to_string(),
                    line: line_num
                }
            );
            let scope;
            if let Some(_) = tokens.peek() {
                let expr =
                    parse_expression(tokens, line_num, next_lines, indentation, pure, filename)?;
                scope = new_scope_with_single_expr(expr, true);
            } else {
                scope = Box::new(parse_scope(next_lines, indentation.into(), true, filename)?);
            }

            Expression::Value(Value::Function { params, scope })
        }

        Token::If => {
            let condition = Rc::new(parse_expression(
                tokens,
                line_num,
                next_lines,
                indentation,
                pure,
                filename,
            )?);
            assert_next_token!(
                tokens,
                Token::Then,
                ParserError {
                    kind: ParserErrorKind::ThenExpected,
                    filename: filename.to_string(),
                    line: line_num
                }
            );
            let then_scope;
            let else_scope;
            // check for then_expression on the same line
            if let Some(_) = tokens.peek() {
                let expr =
                    parse_expression(tokens, line_num, next_lines, indentation, pure, filename)?;
                then_scope = new_scope_with_single_expr(expr, pure);
                // if yes, check for else_expression on the same line
                if let Some(_) = tokens.peek() {
                    assert_next_token!(
                        tokens,
                        Token::Else,
                        ParserError {
                            kind: ParserErrorKind::ElseExpected,
                            filename: filename.to_string(),
                            line: line_num
                        }
                    );
                    let expr = parse_expression(
                        tokens,
                        line_num,
                        next_lines,
                        indentation,
                        pure,
                        filename,
                    )?;
                    else_scope = new_scope_with_single_expr(expr, pure);

                    // if both - return
                    return Ok(Expression::If {
                        condition,
                        then_scope,
                        else_scope,
                    });
                }
            } else {
                // if not on the same line, parse scope
                then_scope = Box::new(parse_scope(next_lines, indentation.into(), pure, filename)?);
            }

            // if then_expression is not on the same line and else in not also
            // take next line and scan it for else

            let next_line = next_lines
                .next()
                .ok_or(ParserError {
                    kind: ParserErrorKind::UnexpectedEOF,
                    filename: filename.to_string(),
                    line: line_num,
                })?
                .map_err(|_| ParserError {
                    kind: ParserErrorKind::LexerError,
                    filename: filename.to_string(),
                    line: line_num,
                })?;
            if next_line.indentation != indentation {
                return Err(ParserError {
                    kind: ParserErrorKind::IndentationError {
                        msg: String::from("Indentation of else should be the same as if!"),
                        expected: indentation as i32,
                        actual: next_line.indentation as i32,
                    },
                    filename: filename.to_string(),
                    line: next_line.number,
                });
            }
            let mut next_line_tokens = next_line.tokens.as_slice().into_iter().peekable();
            assert_next_token!(
                next_line_tokens,
                Token::Else,
                ParserError {
                    kind: ParserErrorKind::ElseExpected,
                    filename: filename.to_string(),
                    line: line_num
                }
            );

            // check if else_expression is on the same line
            let else_scope;
            if let Some(_) = next_line_tokens.peek() {
                let expr = parse_expression(
                    &mut next_line_tokens,
                    next_line.number,
                    next_lines,
                    next_line.indentation,
                    pure,
                    filename,
                )?;
                else_scope = new_scope_with_single_expr(expr, pure);
            } else {
                else_scope = Box::new(parse_scope(next_lines, indentation.into(), pure, filename)?);
            }

            Expression::If {
                condition,
                then_scope,
                else_scope,
            }
        }
        Token::LeftBracket => {
            let expr = parse_expression(tokens, line_num, next_lines, indentation, pure, filename)?;
            assert_next_token!(
                tokens,
                Token::RightBracket,
                ParserError {
                    kind: ParserErrorKind::UnbalancedBracketsError,
                    filename: filename.to_string(),
                    line: line_num
                }
            );
            expr
        }
        token => {
            return Err(ParserError {
                kind: ParserErrorKind::UnexpectedToken {
                    token: token.clone(),
                },
                filename: filename.to_string(),
                line: line_num,
            })
        }
    };

    match tokens.peek() {
        Some(Token::Operation(op)) => {
            tokens.next().unwrap();
            let expr_2 =
                parse_expression(tokens, line_num, next_lines, indentation, pure, filename)?;
            parse_binary_operation(op, expr, expr_2, line_num, filename)
        }
        Some(Token::LeftBracket) => {
            tokens.next().unwrap();
            let expr_vec =
                parse_args_list(tokens, line_num, next_lines, indentation, pure, filename)?;
            Ok(Expression::FunctionCall {
                name: Rc::new(expr),
                args: expr_vec,
            })
        }
        _ => Ok(expr),
    }
}

#[derive(Debug)]
enum FunctionLine {
    Expression(Expression),
    Assignment(String, Expression),
    Import(String),
    Empty,
}

fn parse_line(
    line: &Line,
    next_lines: &mut Peekable<LinesIterator>,
    indentation: u16,
    pure: bool,
    filename: &str,
) -> Result<FunctionLine, ParserError> {
    let tokens = &line.tokens;

    if tokens.len() == 0 {
        Ok(FunctionLine::Empty)
    } else if tokens.len() == 2 && matches!(tokens[0], Token::Import) {
        match &tokens[1] {
            Token::Str(string) => Ok(FunctionLine::Import(string.clone())),
            _ => Err(ParserError {
                kind: ParserErrorKind::FilenameStringExpected,
                filename: filename.to_string(),
                line: line.number,
            }),
        }
    } else if tokens.len() > 2 && matches!(tokens[1], Token::Assignment) {
        match &tokens[0] {
            Token::Name(string) => {
                let mut iter = tokens[2..].iter().peekable();
                let expression = parse_expression(
                    &mut iter,
                    line.number,
                    next_lines,
                    indentation,
                    pure,
                    filename,
                )?;
                Ok(FunctionLine::Assignment(string.clone(), expression))
            }
            _ => Err(ParserError {
                kind: ParserErrorKind::AssignmentError {
                    msg: String::from(
                        "The token on the left side of the assignment is not a name!",
                    ),
                },
                filename: filename.to_string(),
                line: line.number,
            }),
        }
    } else {
        let mut iter = tokens.iter().peekable();
        parse_expression(
            &mut iter,
            line.number,
            next_lines,
            indentation,
            pure,
            filename,
        )
        .map(|exp| FunctionLine::Expression(exp))
    }
}

fn handle_line(
    line: &Line,
    expressions: &mut Vec<Expression>,
    assignments: &mut Vec<(String, Rc<Expression>)>,
    next_lines: &mut Peekable<LinesIterator>,
    indentation: u16,
    pure: bool,
    filename: &str,
) -> Result<(), ParserError> {
    let parsed = parse_line(line, next_lines, indentation, pure, filename)?;
    match parsed {
        FunctionLine::Expression(exp) => {
            if pure && expressions.len() > 0 {
                return Err(ParserError {
                    kind: ParserErrorKind::ReturnExpressionError {
                        msg: String::from("A pure scope can only have 1 expression!"),
                    },
                    filename: filename.to_string(),
                    line: line.number,
                });
            } else {
                expressions.push(exp);
                Ok(())
            }
        }
        FunctionLine::Assignment(string, exp) => {
            if pure && expressions.len() == 1 {
                return Err(ParserError {
                    kind: ParserErrorKind::ReturnExpressionError {
                        msg: String::from(
                            "Cannot have assignments after expression in a single pure scope!",
                        ),
                    },
                    filename: filename.to_string(),
                    line: line.number,
                });
            } else {
                assignments.push((string, Rc::new(exp)));
                Ok(())
            }
        }
        FunctionLine::Import(import_filename) => {
            let import_path = match PathBuf::from(filename).parent() {
                Some(parent) => parent.join(import_filename),
                None => PathBuf::from(import_filename),
            };
            let import_path_str = import_path.to_string_lossy().to_string();
            let file = File::open(import_path.clone()).or(Err(ParserError {
                kind: ParserErrorKind::CannotImportFile {
                    import_filename: import_path_str.clone(),
                },
                filename: filename.to_string(),
                line: line.number,
            }))?;

            let lines = lines(file);
            let scope = parse(lines, &import_path_str)?;

            match scope {
                Scope::Impure {
                    assignments: mut import_assignments,
                    statements,
                } => {
                    assignments.append(&mut import_assignments);
                    if !statements.is_empty() {
                        return Err(ParserError {
                            kind: ParserErrorKind::UnexpectedExpressionInTopLevelOfImport,
                            filename: filename.to_string(),
                            line: line.number,
                        });
                    }
                }
                Scope::Pure { .. } => panic!("Unexpected Pure scope in import"),
            };

            Ok(())
        }
        FunctionLine::Empty => Ok(()),
    }
}

fn parse_scope(
    lines: &mut Peekable<LinesIterator>,
    outside_indentation: i32,
    pure: bool,
    filename: &str,
) -> Result<Scope, ParserError> {
    let mut assignments = Vec::<(String, Rc<Expression>)>::new();
    let mut expressions: Vec<Expression> = vec![];
    let mut last_line_number;

    let line = lines
        .next()
        .ok_or(ParserError {
            kind: ParserErrorKind::UnexpectedEOF,
            filename: filename.to_string(),
            line: 0,
        })?
        .map_err(|_| ParserError {
            kind: ParserErrorKind::LexerError,
            filename: filename.to_string(),
            line: 0,
        })?;

    last_line_number = line.number;

    let scope_indentation = line.indentation;
    if scope_indentation as i32 <= outside_indentation {
        return Err(ParserError {
            kind: ParserErrorKind::IndentationError {
                msg: String::from("Scope has to be indented"),
                expected: outside_indentation + 1,
                actual: outside_indentation,
            },
            filename: filename.to_string(),
            line: line.number,
        });
    }

    handle_line(
        &line,
        &mut expressions,
        &mut assignments,
        lines,
        scope_indentation,
        pure,
        filename,
    )?;

    while let Some(line_result) = lines.peek() {
        let next_line = line_result.as_ref().map_err(|_| ParserError {
            kind: ParserErrorKind::PeekError,
            filename: filename.to_string(),
            line: last_line_number,
        })?;
        last_line_number = next_line.number;

        if next_line.indentation < scope_indentation
            && next_line.indentation as i32 <= outside_indentation
        {
            // end of scope
            break;
        } else if outside_indentation > 0 && next_line.indentation < scope_indentation {
            // scope has not ended yet but has different indentation
            return Err(ParserError {
                kind: ParserErrorKind::IndentationError {
                    msg: String::from("The indentation of a scope should be the same"),
                    expected: scope_indentation as i32,
                    actual: line.indentation as i32,
                },
                filename: filename.to_string(),
                line: last_line_number,
            });
        } else {
            // scope has not ended and has valid indentation
            let line = lines.next().unwrap().map_err(|_| ParserError {
                kind: ParserErrorKind::LexerError,
                filename: filename.to_string(),
                line: last_line_number,
            })?;

            handle_line(
                &line,
                &mut expressions,
                &mut assignments,
                lines,
                scope_indentation,
                pure,
                filename,
            )?;
        }
    }

    if pure {
        if expressions.len() == 0 {
            Err(ParserError {
                kind: ParserErrorKind::ExpressionExpected,
                filename: filename.to_string(),
                line: last_line_number,
            })
        } else {
            if expressions.len() != 1 {
                panic!("Expressions must be no more than one in pure scope");
            }
            Ok(Scope::Pure {
                assignments,
                expression: Rc::new(expressions.pop().unwrap()),
            })
        }
    } else {
        Ok(Scope::Impure {
            assignments,
            statements: expressions.into_iter().map(Rc::new).collect(),
        })
    }
}

pub fn parse(lines: LinesIterator, filename: &str) -> Result<Scope, ParserError> {
    let mut lines = lines.peekable();
    parse_scope(&mut lines, -1, false, filename)
}
