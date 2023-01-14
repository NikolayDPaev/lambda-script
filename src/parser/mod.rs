pub mod enums;
pub mod errors;
mod tests;
use std::iter::Peekable;
use std::rc::Rc;
use std::slice::Iter;
use std::vec;

use crate::lexer::*;
use crate::parser::enums::*;
use crate::parser::errors::*;

macro_rules! new_binary_operation {
    ($op:expr, $expr_1:expr, $expr_2:expr) => {
        Expression::BinaryOperation($op, Rc::new($expr_1), Rc::new($expr_2))
    };
}

// macro_rules! assert_no_more_tokens {
//     ($tokens:expr, $line_num:expr) => {
//         if let Some(token) = $tokens.peek() {
//             return Err(ParserError::UnexpectedToken{line: $line_num, token: (*token).clone()});
//         }
//     };
// }

fn parse_binary_operation(
    op: &Op,
    expr_1: Expression,
    expr_2: Expression,
    line_num: u32,
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
            new_binary_operation!(BinaryOp::Arithmetic(ArithBinOp::IntDivision), expr_1, expr_2)
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
        Op::Modulo => new_binary_operation!(BinaryOp::Arithmetic(ArithBinOp::Modulo), expr_1, expr_2),
        Op::Eq => new_binary_operation!(BinaryOp::Compare(CmpBinOp::Eq), expr_1, expr_2),
        Op::NEq => new_binary_operation!(BinaryOp::Compare(CmpBinOp::NEq), expr_1, expr_2),
        Op::Lt => new_binary_operation!(BinaryOp::Compare(CmpBinOp::Lt), expr_1, expr_2),
        Op::Gt => new_binary_operation!(BinaryOp::Compare(CmpBinOp::Gt), expr_1, expr_2),
        Op::LEq => new_binary_operation!(BinaryOp::Compare(CmpBinOp::LEq), expr_1, expr_2),
        Op::GEq => new_binary_operation!(BinaryOp::Compare(CmpBinOp::GEq), expr_1, expr_2),
        Op::Negation => return Err(ParserError::OpError { line: line_num }),
    })
}

fn parse_string(string: &str) -> Value {
    if string.len() == 0 {
        Value::Nil
    } else {
        Value::Tuple(
            Box::new(Value::Char(
                string.chars().next().unwrap(),
            )),
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

fn parse_args_list(
    tokens: &mut Peekable<Iter<Token>>,
    line_num: u32,
    next_lines: &mut Peekable<LinesIterator>,
    indentation: u16,
) -> Result<Vec<Rc<Expression>>, ParserError> {
    let mut vec = vec![];

    loop {
        let token = tokens
            .peek()
            .ok_or(ParserError::UnexpectedEndOfLine { line: line_num })?;

        match token {
            Token::RightBracket => {
                tokens.next().unwrap();
                return Ok(vec);
            }
            _ => {
                let expr = parse_expression(tokens, line_num, next_lines, indentation)?;
                vec.push(Rc::new(expr));
                match tokens.peek() {
                    Some(Token::Comma) => {
                        tokens.next().unwrap();
                    }
                    Some(Token::RightBracket) => (),
                    _ => return Err(ParserError::CommaExpected { line: line_num }),
                };
            }
        };
    }
}

fn parse_param_list(
    tokens: &mut Peekable<Iter<Token>>,
    line_num: u32,
) -> Result<Vec<String>, ParserError> {
    let mut vec = vec![];

    loop {
        let token = tokens
            .next()
            .ok_or(ParserError::UnexpectedEndOfLine { line: line_num })?;
        match token {
            Token::RightBoxBracket => return Ok(vec),
            Token::Name(string) => {
                vec.push(string.clone());
                match tokens.peek() {
                    Some(Token::Comma) => {
                        tokens.next().unwrap();
                    }
                    Some(Token::RightBoxBracket) => (),
                    _ => return Err(ParserError::CommaExpected { line: line_num }),
                };
            }
            _ => return Err(ParserError::NameExpected { line: line_num }),
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
) -> Result<Expression, ParserError> {
    let token = tokens
        .next()
        .ok_or(ParserError::ExpressionExpected { line: line_num })?;
    let expr = match token {
        Token::Name(string) => {
            if let Some(Token::LeftBracket) = tokens.peek() {
                tokens.next().unwrap();
                let expr_vec = parse_args_list(tokens, line_num, next_lines, indentation)?;
                Expression::FunctionCall {
                    name: Rc::new(Expression::Name(string.clone())),
                    args: expr_vec,
                }
            } else {
                Expression::Name(string.clone())
            }
        }
        Token::Number(string) => parse_number(string, line_num)?,
        Token::Str(string) => Expression::Value(parse_string(string)),
        Token::Char(string) => {
            if string.len() != 1 {
                return Err(ParserError::CharParseError { line: line_num });
            } else {
                Expression::Value(Value::Char(string.chars().next().unwrap()))
            }
        }
        Token::True => Expression::Value(Value::Boolean(true)),
        Token::False => Expression::Value(Value::Boolean(false)),
        Token::Operation(Op::Minus) => {
            let expr = parse_expression(tokens, line_num, next_lines, indentation)?;
            Expression::UnaryOperation(UnaryOp::Minus, Rc::new(expr))
        }
        Token::Operation(Op::Negation) => {
            let expr = parse_expression(tokens, line_num, next_lines, indentation)?;
            Expression::UnaryOperation(UnaryOp::Negation, Rc::new(expr))
        }
        Token::Nil => Expression::Value(Value::Nil),
        Token::Cons => {
            assert_next_token!(
                tokens,
                Token::LeftBracket,
                ParserError::LeftBracketExpected { line: line_num }
            );
            let expr_1 = Rc::new(parse_expression(tokens, line_num, next_lines, indentation)?);
            assert_next_token!(
                tokens,
                Token::Comma,
                ParserError::CommaExpected { line: line_num }
            );
            let expr_2 = Rc::new(parse_expression(tokens, line_num, next_lines, indentation)?);
            assert_next_token!(
                tokens,
                Token::RightBracket,
                ParserError::UnbalancedBracketsError { line: line_num }
            );
            Expression::Cons(expr_1, expr_2)
        }
        Token::Left => {
            assert_next_token!(
                tokens,
                Token::LeftBracket,
                ParserError::LeftBracketExpected { line: line_num }
            );
            let expr = Rc::new(parse_expression(tokens, line_num, next_lines, indentation)?);
            assert_next_token!(
                tokens,
                Token::RightBracket,
                ParserError::UnbalancedBracketsError { line: line_num }
            );
            Expression::Left(expr)
        }
        Token::Right => {
            assert_next_token!(
                tokens,
                Token::LeftBracket,
                ParserError::LeftBracketExpected { line: line_num }
            );
            let expr = Rc::new(parse_expression(tokens, line_num, next_lines, indentation)?);
            assert_next_token!(
                tokens,
                Token::RightBracket,
                ParserError::UnbalancedBracketsError { line: line_num }
            );
            Expression::Right(expr)
        }
        Token::Empty => {
            assert_next_token!(
                tokens,
                Token::LeftBracket,
                ParserError::LeftBracketExpected { line: line_num }
            );
            let expr = Rc::new(parse_expression(tokens, line_num, next_lines, indentation)?);
            assert_next_token!(
                tokens,
                Token::RightBracket,
                ParserError::UnbalancedBracketsError { line: line_num }
            );
            Expression::Empty(expr)
        }
        Token::Print => {
            assert_next_token!(
                tokens,
                Token::LeftBracket,
                ParserError::LeftBracketExpected { line: line_num }
            );
            let expr = Rc::new(parse_expression(tokens, line_num, next_lines, indentation)?);
            assert_next_token!(
                tokens,
                Token::RightBracket,
                ParserError::UnbalancedBracketsError { line: line_num }
            );
            Expression::PrintCall(expr)
        }
        Token::Read => Expression::ReadCall,
        Token::NonPure => {
            let mut params = Vec::new();
            if let Some(Token::LeftBoxBracket) = tokens.peek() {
                tokens.next().unwrap();
                params = parse_param_list(tokens, line_num)?;
            }
            assert_next_token!(
                tokens,
                Token::Arrow,
                ParserError::ArrowExpected { line: line_num }
            );
            let scope = Box::new(parse_scope(next_lines, indentation.into(), false)?);
            
            Expression::Value(Value::Function {
                params,
                scope,
            })
        }
        Token::Arrow => {
            let scope = Box::new(parse_scope(next_lines, indentation.into(), true)?);
            Expression::Value(Value::Function {
                params: vec![],
                scope,
            })
        }
        Token::LeftBoxBracket => {
            let params = parse_param_list(tokens, line_num)?;
            assert_next_token!(
                tokens,
                Token::Arrow,
                ParserError::ArrowExpected { line: line_num }
            );
            let scope = Box::new(parse_scope(next_lines, indentation.into(), true)?);
            
            Expression::Value(Value::Function {
                params,
                scope,
            })
        }

        Token::If => {
            let condition = Rc::new(parse_expression(tokens, line_num, next_lines, indentation)?);
            assert_next_token!(
                tokens,
                Token::Then,
                ParserError::ThenExpected { line: line_num }
            );
            
            let then_scope = Box::new(parse_scope(next_lines, indentation.into(), true)?);
            let line = next_lines
                .next()
                .ok_or(ParserError::UnexpectedEOF)?
                .map_err(|_| ParserError::LexerError)?;
            if line.indentation != indentation {
                return Err(ParserError::IndentationError {
                    line: line_num,
                    msg: String::from("Indentation of else should be the same as if!"),
                    expected: indentation as i32,
                    actual: line.indentation as i32,
                });
            }
            assert_next_token!(
                line.tokens.into_iter(),
                Token::Else,
                ParserError::ElseExpected { line: line_num }
            );

            let else_scope = Box::new(parse_scope(next_lines, indentation.into(), true)?);
            Expression::If {
                condition,
                then_scope,
                else_scope,
            }
        }
        Token::LeftBracket => {
            let expr = parse_expression(tokens, line_num, next_lines, indentation)?;
            assert_next_token!(
                tokens,
                Token::RightBracket,
                ParserError::UnbalancedBracketsError { line: line_num }
            );
            expr
        },
        token => {
            return Err(ParserError::UnexpectedToken {
                line: line_num,
                token: token.clone(),
            })
        }
    };

    if let Some(Token::Operation(op)) = tokens.peek() {
        tokens.next().unwrap();
        let expr_2 = parse_expression(tokens, line_num, next_lines, indentation)?;
        parse_binary_operation(op, expr, expr_2, line_num)
    } else {
        Ok(expr)
    }
}

enum FunctionLine {
    Expression(Expression),
    Assignment(String, Expression),
    Empty,
}

fn parse_line(
    line: &Line,
    next_lines: &mut Peekable<LinesIterator>,
    indentation: u16,
) -> Result<FunctionLine, ParserError> {
    let tokens = &line.tokens;

    if tokens.len() == 0 {
        return Ok(FunctionLine::Empty);
    }

    if tokens.len() > 2 && matches!(tokens[1], Token::Assignment) {
        match &tokens[0] {
            Token::Name(string) => {
                let mut iter = tokens[2..].iter().peekable();
                let expression = parse_expression(&mut iter, line.number, next_lines, indentation)?;
                Ok(FunctionLine::Assignment(string.clone(), expression))
            }
            _ => Err(ParserError::AssignmentError {
                line: line.number,
                msg: String::from("The token on the left side of the assignment is not a name!"),
            }),
        }
    } else {
        let mut iter = tokens.iter().peekable();
        parse_expression(&mut iter, line.number, next_lines, indentation)
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
) -> Result<(), ParserError> {
    match parse_line(line, next_lines, indentation)? {
        FunctionLine::Expression(exp) => {
            if pure && expressions.len() > 0 {
                return Err(ParserError::ReturnExpressionError {
                    line: line.number,
                    msg: String::from("A pure scope can only have 1 expression!"),
                })
            } else {
                expressions.push(exp);
                Ok(())
            }            
        },
        FunctionLine::Assignment(string, exp) => {
            if pure && expressions.len() == 1 {
                return Err(ParserError::ReturnExpressionError {
                    line: line.number,
                    msg: String::from(
                        "Cannot have assignments after expression in a single pure scope!",
                    ),
                })
            } else {
                assignments.push((string, Rc::new(exp)));
                Ok(())
            }
        },
        FunctionLine::Empty => Ok(()),
    }
}

fn parse_scope(
    lines: &mut Peekable<LinesIterator>,
    outside_indentation: i32,
    pure: bool,
) -> Result<Scope, ParserError> {    
    let mut assignments = Vec::<(String, Rc<Expression>)>::new();
    let mut expressions: Vec<Expression> = vec![];
    let mut last_line_number;

    let line = lines
        .next()
        .ok_or(ParserError::UnexpectedEOF)?
        .map_err(|_| ParserError::LexerError)?;

    last_line_number = line.number;

    let scope_indentation = line.indentation;   
    if scope_indentation as i32 <= outside_indentation {
        return Err(ParserError::IndentationError {
            line: line.number,
            msg: String::from("Scope has to be indented"),
            expected: outside_indentation + 1,
            actual: outside_indentation,
        });
    }

    handle_line(
        &line,
        &mut expressions,
        &mut assignments,
        lines,
        scope_indentation,
        pure
    )?;

    while let Some(line_result) = lines.peek() {
        let next_line = line_result.as_ref().map_err(|_| ParserError::PeekError)?;
        last_line_number = next_line.number;

        if next_line.indentation < scope_indentation && next_line.indentation as i32 <= outside_indentation {
            // end of scope
            break;
        } else if outside_indentation > 0 && next_line.indentation < scope_indentation {
            // scope has not ended yet but has different indentation
            return Err(ParserError::IndentationError {
                line: last_line_number,
                msg: String::from("The indentation of a scope should be the same"),
                expected: scope_indentation as i32,
                actual: line.indentation as i32,
            });
        } else {
            // scope has not ended and has valid indentation
            let line = lines.next().unwrap().map_err(|_| ParserError::LexerError)?;
        
            handle_line(
                &line,
                &mut expressions,
                &mut assignments,
                lines,
                scope_indentation,
                pure
            )?;
        }
    }

    if pure {
        if expressions.len() == 0 {
            Err(ParserError::ExpressionExpected { line: last_line_number })
        } else {
            if expressions.len() != 1 {
                panic!("Expressions must be no more than one in pure scope");
            }
            Ok(Scope::Pure { assignments, expression: Rc::new(expressions.pop().unwrap()) })
        }
    } else {
        Ok(Scope::NonPure { assignments, statements: expressions.into_iter().map(Rc::new).collect() })
    }
}

pub fn parse(lines: LinesIterator) -> Result<Scope, ParserError> {
    let mut lines = lines.peekable();
    parse_scope(&mut lines, -1, false)
}
