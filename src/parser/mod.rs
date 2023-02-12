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

macro_rules! assert_next_token {
    ($tokens:expr, $token:pat, $error:expr) => {
        match $tokens.next() {
            Some($token) => (),
            _ => return Err($error),
        };
    };
}

#[derive(Debug, PartialEq, Clone)]
pub enum FunctionLine {
    Expression(Rc<Expression>),
    Assignment(String, Rc<Expression>),
}

#[derive(Debug)]
enum ParsedLine {
    Expression(Expression),
    Assignment(String, Expression),
    Import(String),
    Empty,
}

pub struct Parser {
    next_lines: Peekable<LinesIterator>,
    filename: String,
}

impl Parser {
    pub fn new(lines: LinesIterator, filename: &str) -> Parser {
        Parser {
            next_lines: lines.peekable(),
            filename: filename.to_owned(),
        }
    }

    pub fn parse_outside_scope(&mut self) -> Result<Scope, ParserError> {
        self.parse_scope(-1, false)
    }

    fn produce_error(&self, kind: ParserErrorKind, line: u32) -> ParserError {
        ParserError {
            kind,
            filename: self.filename.clone(),
            line,
        }
    }

    fn parse_scope(&mut self, outside_indentation: i32, pure: bool) -> Result<Scope, ParserError> {
        let mut lines: Vec<FunctionLine> = vec![];
        let mut last_line_number;

        let line = self
            .next_lines
            .next()
            .ok_or(self.produce_error(ParserErrorKind::UnexpectedEOF, 0))?
            .map_err(|_| self.produce_error(ParserErrorKind::LexerError, 0))?;

        last_line_number = line.number;

        let scope_indentation = line.indentation;
        if scope_indentation as i32 <= outside_indentation {
            return Err(self.produce_error(
                ParserErrorKind::IndentationError {
                    msg: String::from("Scope has to be indented"),
                    expected: outside_indentation + 1,
                    actual: outside_indentation,
                },
                line.number,
            ));
        }

        self.handle_scope_line(
            &line,
            &mut lines,
            scope_indentation,
            pure,
        )?;

        while let Some(line_result) = self.next_lines.peek() {
            let next_line = line_result.as_ref().map_err(|_| ParserError {
                kind: ParserErrorKind::PeekError,
                filename: self.filename.clone(),
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
                return Err(self.produce_error(
                    ParserErrorKind::IndentationError {
                        msg: String::from("The indentation of a scope should be the same"),
                        expected: scope_indentation as i32,
                        actual: line.indentation as i32,
                    },
                    last_line_number,
                ));
            } else {
                // scope has not ended and has valid indentation
                let line = self.next_lines.next().unwrap().map_err(|_| ParserError {
                    kind: ParserErrorKind::LexerError,
                    filename: self.filename.clone(),
                    line: last_line_number,
                })?;

                self.handle_scope_line(
                    &line,
                    &mut lines,
                    scope_indentation,
                    pure,
                )?;
            }
        }

        if pure {
            let mut expressions = vec![];
            let mut assignments = vec![];

            for function_line in lines {
                match function_line {
                    FunctionLine::Assignment(name, expr) =>{
                        if expressions.len() == 1 {
                            return Err(self.produce_error(
                                ParserErrorKind::ReturnExpressionError {
                                    msg: String::from(
                                        "Cannot have assignments after expression in a single pure scope!",
                                    ),
                                },
                                line.number,
                            ));
                        }
                        assignments.push((name, expr));
                    } 
                    FunctionLine::Expression(expr) => {
                        if expressions.len() > 0 {
                            return Err(self.produce_error(
                                ParserErrorKind::ReturnExpressionError {
                                    msg: String::from("A pure scope can only have 1 expression!"),
                                },
                                line.number,
                            ));
                        }
                        expressions.push(expr);
                    }
                }
            }

            if expressions.len() == 0 {
                Err(self.produce_error(ParserErrorKind::ExpressionExpected, last_line_number))
            } else {
                if expressions.len() != 1 {
                    panic!("Expressions must be no more than one in pure scope");
                }
                Ok(Scope::Pure {
                    assignments,
                    expression: expressions.pop().unwrap(),
                })
            }
        } else {
            Ok(Scope::Impure {
                lines
            })
        }
    }

    // mutates the lines vectors of the scope
    fn handle_scope_line(
        &mut self,
        line: &Line,
        lines: &mut Vec<FunctionLine>,
        indentation: u16,
        pure: bool
    ) -> Result<(), ParserError> {
        let parsed = self.parse_line(line, indentation, pure)?;
        match parsed {
            ParsedLine::Expression(exp) => {
                lines.push(FunctionLine::Expression(Rc::new(exp)));
            }
            ParsedLine::Assignment(string, exp) => {
                lines.push(FunctionLine::Assignment(string, Rc::new(exp)));
            }
            ParsedLine::Import(import_filename) => {
                let import_path = match PathBuf::from(self.filename.clone()).parent() {
                    Some(parent) => parent.join(import_filename),
                    None => PathBuf::from(import_filename),
                };
                let import_path_str = import_path.to_string_lossy().to_string();
                let file = File::open(import_path.clone()).map_err(|err| {
                    self.produce_error(
                        ParserErrorKind::CannotImportFile {
                            import_filename: import_path_str.clone(),
                            error_message: err.to_string(),
                        },
                        line.number,
                    )
                })?;

                let scope = Parser::new(crate::lexer::lines(file), &import_path_str).parse_outside_scope()?;

                match scope {
                    Scope::Impure {
                        lines: mut import_lines,
                    } => lines.append(&mut import_lines),
                    Scope::Pure { .. } => panic!("Unexpected Pure scope in import"),
                };
            }
            ParsedLine::Empty => (),
        };
        Ok(())
    }

    // parses the line and returns expression, assignment, import or empty line
    fn parse_line(
        &mut self,
        line: &Line,
        indentation: u16,
        pure: bool,
    ) -> Result<ParsedLine, ParserError> {
        let tokens = &line.tokens;

        if tokens.len() == 0 {
            Ok(ParsedLine::Empty)
        } else if tokens.len() == 2 && matches!(tokens[0], Token::Import) {
            match &tokens[1] {
                Token::Str(string) => Ok(ParsedLine::Import(string.clone())),
                _ => Err(self.produce_error(ParserErrorKind::FilenameStringExpected, line.number)),
            }
        } else if tokens.len() > 2 && matches!(tokens[1], Token::Assignment) {
            match &tokens[0] {
                Token::Name(string) => {
                    let mut iter = tokens[2..].iter().peekable();
                    let expression =
                        self.parse_expression(&mut iter, line.number, indentation, pure)?;
                    Ok(ParsedLine::Assignment(string.clone(), expression))
                }
                _ => Err(self.produce_error(
                    ParserErrorKind::AssignmentError {
                        msg: String::from(
                            "The token on the left side of the assignment is not a name!",
                        ),
                    },
                    line.number,
                )),
            }
        } else {
            let mut iter = tokens.iter().peekable();
            self.parse_expression(&mut iter, line.number, indentation, pure)
                .map(|exp| ParsedLine::Expression(exp))
        }
    }

    fn parse_expression(
        &mut self,
        tokens: &mut Peekable<Iter<Token>>,
        line_num: u32,
        indentation: u16,
        pure: bool,
    ) -> Result<Expression, ParserError> {
        let token = tokens
            .next()
            .ok_or(self.produce_error(ParserErrorKind::ExpressionExpected, line_num))?;
        let expr =
            match token {
                Token::Name(string) => {
                    if let Some(Token::LeftBracket) = tokens.peek() {
                        tokens.next().unwrap();
                        let expr_vec = self.parse_args_list(tokens, line_num, indentation, pure)?;
                        Expression::FunctionCall {
                            name: Rc::new(Expression::Name(string.clone())),
                            args: expr_vec,
                        }
                    } else {
                        Expression::Name(string.clone())
                    }
                }
                Token::Number(string) => parse_number(string, line_num, &self.filename)?,
                Token::Str(string) => Expression::Value(parse_string(string)),
                Token::Char(string) => {
                    if string.chars().count() != 1 {
                        return Err(self.produce_error(ParserErrorKind::CharParseError, line_num));
                    } else {
                        Expression::Value(Value::Char(string.chars().next().unwrap()))
                    }
                }
                Token::True => Expression::Value(Value::Boolean(true)),
                Token::False => Expression::Value(Value::Boolean(false)),
                Token::Operation(Op::Minus) => {
                    let expr = self.parse_expression(tokens, line_num, indentation, pure)?;
                    Expression::UnaryOperation(UnaryOp::Minus, Rc::new(expr))
                }
                Token::Operation(Op::Negation) => {
                    let expr = self.parse_expression(tokens, line_num, indentation, pure)?;
                    Expression::UnaryOperation(UnaryOp::Negation, Rc::new(expr))
                }
                Token::Nil => Expression::Value(Value::Nil),
                Token::Cons => {
                    let (expr_1, expr_2) =
                        self.parse_built_in_binary_function_call(tokens, line_num, indentation, pure)?;
                    Expression::Cons(Rc::new(expr_1), Rc::new(expr_2))
                }
                Token::Left => Expression::Left(Rc::new(self.parse_built_in_unary_function_call(
                    tokens,
                    line_num,
                    indentation,
                    pure,
                )?)),
                Token::Right => Expression::Right(Rc::new(self.parse_built_in_unary_function_call(
                    tokens,
                    line_num,
                    indentation,
                    pure,
                )?)),
                Token::Empty => Expression::Empty(Rc::new(self.parse_built_in_unary_function_call(
                    tokens,
                    line_num,
                    indentation,
                    pure,
                )?)),
                Token::Print => Expression::PrintCall(Rc::new(
                    self.parse_built_in_unary_function_call(tokens, line_num, indentation, pure)?,
                )),
                Token::Read => Expression::ReadCall,
                Token::Impure => {
                    let mut params = Vec::new();
                    if let Some(Token::LeftBoxBracket) = tokens.peek() {
                        tokens.next().unwrap();
                        params = parse_name_list(tokens, line_num, &self.filename)?;
                    }
                    assert_next_token!(
                        tokens,
                        Token::Arrow,
                        self.produce_error(ParserErrorKind::ArrowExpected, line_num)
                    );

                    let scope;
                    if let Some(_) = tokens.peek() {
                        let expr = self.parse_expression(tokens, line_num, indentation, false)?;
                        scope = new_scope_with_single_expr(expr, false);
                    } else {
                        scope = Box::new(self.parse_scope(indentation.into(), false)?);
                    }

                    Expression::Value(Value::Function { params, scope })
                }
                Token::Arrow => {
                    let scope;
                    if let Some(_) = tokens.peek() {
                        let expr = self.parse_expression(tokens, line_num, indentation, pure)?;
                        scope = new_scope_with_single_expr(expr, true);
                    } else {
                        scope = Box::new(self.parse_scope(indentation.into(), true)?);
                    }
                    Expression::Value(Value::Function {
                        params: vec![],
                        scope,
                    })
                }
                Token::LeftBoxBracket => {
                    let params = parse_name_list(tokens, line_num, &self.filename)?;
                    assert_next_token!(
                        tokens,
                        Token::Arrow,
                        self.produce_error(ParserErrorKind::ArrowExpected, line_num)
                    );
                    let scope;
                    if let Some(_) = tokens.peek() {
                        let expr = self.parse_expression(tokens, line_num, indentation, pure)?;
                        scope = new_scope_with_single_expr(expr, true);
                    } else {
                        scope = Box::new(self.parse_scope(indentation.into(), true)?);
                    }

                    Expression::Value(Value::Function { params, scope })
                }

                Token::If => {
                    let condition =
                        Rc::new(self.parse_expression(tokens, line_num, indentation, pure)?);
                    assert_next_token!(
                        tokens,
                        Token::Then,
                        self.produce_error(ParserErrorKind::ThenExpected, line_num)
                    );
                    let then_scope;
                    let else_scope;
                    // check for then_expression on the same line
                    if let Some(_) = tokens.peek() {
                        let expr = self.parse_expression(tokens, line_num, indentation, pure)?;
                        then_scope = new_scope_with_single_expr(expr, pure);
                        // if yes, check for else_expression on the same line
                        if let Some(_) = tokens.peek() {
                            assert_next_token!(
                                tokens,
                                Token::Else,
                                self.produce_error(ParserErrorKind::ElseExpected, line_num)
                            );
                            let expr =
                                self.parse_expression(tokens, line_num, indentation, pure)?;
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
                        then_scope = Box::new(self.parse_scope(indentation.into(), pure)?);
                    }
                    // if then_expression is not on the same line and else in not also
                    // take next line and scan it for else
                    let next_line = self
                        .next_lines
                        .next()
                        .ok_or(self.produce_error(ParserErrorKind::UnexpectedEOF, line_num))?
                        .map_err(|_| self.produce_error(ParserErrorKind::LexerError, line_num))?;
                    if next_line.indentation != indentation {
                        return Err(self.produce_error(
                            ParserErrorKind::IndentationError {
                                msg: String::from("Indentation of else should be the same as if!"),
                                expected: indentation as i32,
                                actual: next_line.indentation as i32,
                            },
                            next_line.number,
                        ));
                    }
                    let mut next_line_tokens = next_line.tokens.as_slice().into_iter().peekable();
                    assert_next_token!(
                        next_line_tokens,
                        Token::Else,
                        self.produce_error(ParserErrorKind::ElseExpected, line_num)
                    );

                    // check if else_expression is on the same line
                    let else_scope;
                    if let Some(_) = next_line_tokens.peek() {
                        let expr = self.parse_expression(
                            &mut next_line_tokens,
                            next_line.number,
                            next_line.indentation,
                            pure,
                        )?;
                        else_scope = new_scope_with_single_expr(expr, pure);
                    } else {
                        else_scope = Box::new(self.parse_scope(indentation.into(), pure)?);
                    }

                    Expression::If {
                        condition,
                        then_scope,
                        else_scope,
                    }
                }
                Token::LeftBracket => {
                    let expr = self.parse_expression(tokens, line_num, indentation, pure)?;
                    assert_next_token!(
                        tokens,
                        Token::RightBracket,
                        self.produce_error(ParserErrorKind::UnbalancedBracketsError, line_num)
                    );
                    expr
                }
                token => {
                    return Err(self.produce_error(
                        ParserErrorKind::UnexpectedToken {
                            token: token.clone(),
                        },
                        line_num,
                    ))
                }
            };

        match tokens.peek() {
            Some(Token::Operation(op)) => {
                tokens.next().unwrap();
                let expr_2 = self.parse_expression(tokens, line_num, indentation, pure)?;
                parse_binary_operation(op, expr, expr_2, line_num, &self.filename)
            }
            Some(Token::LeftBracket) => {
                tokens.next().unwrap();
                let expr_vec = self.parse_args_list(tokens, line_num, indentation, pure)?;
                Ok(Expression::FunctionCall {
                    name: Rc::new(expr),
                    args: expr_vec,
                })
            }
            _ => Ok(expr),
        }
    }

    // parses "name(arg)" function call
    fn parse_built_in_unary_function_call(
        &mut self,
        tokens: &mut Peekable<Iter<Token>>,
        line_num: u32,
        indentation: u16,
        pure: bool,
    ) -> Result<Expression, ParserError> {
        assert_next_token!(
            tokens,
            Token::LeftBracket,
            self.produce_error(ParserErrorKind::LeftBracketExpected, line_num)
        );
        let expr = self.parse_expression(tokens, line_num, indentation, pure)?;
        assert_next_token!(
            tokens,
            Token::RightBracket,
            self.produce_error(ParserErrorKind::UnbalancedBracketsError, line_num)
        );
        Ok(expr)
    }

    // parses "name(arg, arg)" function call
    fn parse_built_in_binary_function_call(
        &mut self,
        tokens: &mut Peekable<Iter<Token>>,
        line_num: u32,
        indentation: u16,
        pure: bool,
    ) -> Result<(Expression, Expression), ParserError> {
        assert_next_token!(
            tokens,
            Token::LeftBracket,
            self.produce_error(ParserErrorKind::LeftBracketExpected, line_num)
        );
        let expr_1 = self.parse_expression(tokens, line_num, indentation, pure)?;
        assert_next_token!(
            tokens,
            Token::Comma,
            self.produce_error(ParserErrorKind::CommaExpected, line_num)
        );
        let expr_2 = self.parse_expression(tokens, line_num, indentation, pure)?;
        assert_next_token!(
            tokens,
            Token::RightBracket,
            self.produce_error(ParserErrorKind::UnbalancedBracketsError, line_num)
        );
        Ok((expr_1, expr_2))
    }

    // parses arbitrary list of args, ending with right bracket
    fn parse_args_list(
        &mut self,
        tokens: &mut Peekable<Iter<Token>>,
        line_num: u32,
        indentation: u16,
        pure: bool,
    ) -> Result<Vec<Rc<Expression>>, ParserError> {
        let mut vec = vec![];

        loop {
            let token = tokens
                .peek()
                .ok_or(self.produce_error(ParserErrorKind::UnexpectedEndOfLine, line_num))?;

            match token {
                Token::RightBracket => {
                    tokens.next().unwrap();
                    return Ok(vec);
                }
                _ => {
                    let expr = self.parse_expression(tokens, line_num, indentation, pure)?;
                    vec.push(Rc::new(expr));
                    match tokens.peek() {
                        Some(Token::Comma) => {
                            tokens.next().unwrap();
                        }
                        Some(Token::RightBracket) => (),
                        _ => {
                            return Err(self.produce_error(ParserErrorKind::CommaExpected, line_num))
                        }
                    };
                }
            };
        }
    }
}

fn new_scope_with_single_expr(expr: Expression, pure: bool) -> Box<Scope> {
    if pure {
        Box::new(Scope::Pure {
            assignments: vec![],
            expression: Rc::new(expr),
        })
    } else {
        Box::new(Scope::Impure {
            lines: vec![FunctionLine::Expression(Rc::new(expr))]
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

fn parse_name_list(
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
