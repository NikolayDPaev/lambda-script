use std::rc::Rc;

use crate::lexer::enums::*;
use crate::lexer::Line;
use crate::parser::errors::ParserErrorKind::*;
use crate::parser::*;

fn test_one_line_expression(tokens: Vec<Token>, expression: Expression) {
    let lines: Vec<Result<Line, std::io::Error>> = vec![Ok(Line {
        number: 1,
        indentation: 0,
        tokens,
    })];
    let result = Parser::new(Box::new(lines.into_iter()), PathBuf::new())
        .parse_outside_scope()
        .unwrap();
    assert_eq!(
        result,
        Scope::Impure {
            lines: vec![ImpureLine::Expression(Rc::new(expression))]
        }
    );
}

fn test_one_line_expression_error(
    tokens: Vec<Token>,
    error_kind: ParserErrorKind,
    error_pos: usize,
) {
    let line = Line {
        number: 1,
        indentation: 0,
        tokens,
    };
    let result = Parser::new(Box::new(vec![Ok(line.clone())].into_iter()), PathBuf::new())
        .parse_outside_scope();
    assert_eq!(
        result,
        Err(ParserError {
            kind: error_kind,
            filename: String::from(""),
            line: Some(line),
            token_pos: error_pos
        },)
    );
}

#[test]
fn test_values() {
    test_one_line_expression(vec![Token::True], Expression::Value(Value::Boolean(true)));
    test_one_line_expression(vec![Token::True], Expression::Value(Value::Boolean(true)));
    test_one_line_expression(
        vec![Token::Str(String::from("bar"))],
        Expression::Value(Value::Tuple(
            Box::new(Value::Char('b')),
            Box::new(Value::Tuple(
                Box::new(Value::Char('a')),
                Box::new(Value::Tuple(
                    Box::new(Value::Char('r')),
                    Box::new(Value::Nil),
                )),
            )),
        )),
    );
    test_one_line_expression(
        vec![Token::Number(String::from("123"))],
        Expression::Value(Value::Number(Number::Integer(123))),
    );
    test_one_line_expression_error(
        vec![Token::Number(String::from("1a23"))],
        NumberParseError,
        0,
    );
    test_one_line_expression(
        vec![Token::Number(String::from("123.5"))],
        Expression::Value(Value::Number(Number::Float(123.5))),
    );
    test_one_line_expression_error(
        vec![Token::Number(String::from("1.a23"))],
        NumberParseError,
        0,
    );
    test_one_line_expression(
        vec![Token::Char(String::from("a"))],
        Expression::Value(Value::Char('a')),
    );
    test_one_line_expression_error(vec![Token::Char(String::from("abc"))], CharParseError, 0);
    test_one_line_expression_error(vec![Token::Char(String::from(""))], CharParseError, 0);
    test_one_line_expression(vec![Token::True], Expression::Value(Value::Boolean(true)));
    test_one_line_expression(vec![Token::False], Expression::Value(Value::Boolean(false)));
    test_one_line_expression(vec![Token::Nil], Expression::Value(Value::Nil));
}

#[test]
fn test_builtin_functions() {
    test_one_line_expression(
        vec![
            Token::Cons,
            Token::LeftBracket,
            Token::True,
            Token::Comma,
            Token::False,
            Token::RightBracket,
        ],
        Expression::Cons(
            Rc::new(Expression::Value(Value::Boolean(true))),
            Rc::new(Expression::Value(Value::Boolean(false))),
        ),
    );

    test_one_line_expression_error(
        vec![
            Token::Cons,
            Token::LeftBracket,
            Token::True,
            Token::RightBracket,
        ],
        CommaExpected,
        3,
    );

    test_one_line_expression(
        vec![
            Token::Print,
            Token::LeftBracket,
            Token::True,
            Token::RightBracket,
        ],
        Expression::PrintCall {
            expr: Rc::new(Expression::Value(Value::Boolean(true))),
            newline: false,
        },
    );

    test_one_line_expression(
        vec![
            Token::Println,
            Token::LeftBracket,
            Token::True,
            Token::RightBracket,
        ],
        Expression::PrintCall {
            expr: Rc::new(Expression::Value(Value::Boolean(true))),
            newline: true,
        },
    );

    test_one_line_expression(
        vec![
            Token::Left,
            Token::LeftBracket,
            Token::True,
            Token::RightBracket,
        ],
        Expression::Left(Rc::new(Expression::Value(Value::Boolean(true)))),
    );

    test_one_line_expression(
        vec![
            Token::Right,
            Token::LeftBracket,
            Token::True,
            Token::RightBracket,
        ],
        Expression::Right(Rc::new(Expression::Value(Value::Boolean(true)))),
    );

    test_one_line_expression(
        vec![Token::Read, Token::LeftBracket, Token::RightBracket],
        Expression::ReadCall,
    );
    test_one_line_expression_error(
        vec![
            Token::Read,
            Token::LeftBracket,
            Token::RightBracket,
            Token::True,
        ],
        UnexpectedToken { token: Token::True },
        3,
    );
}

#[test]
fn test_builtin_functions_as_first_class_citizens() {
    test_one_line_expression(
        vec![Token::Cons],
        Expression::Value(Value::Function {
            params: vec![0, 1],
            scope: Box::new(Scope::Pure {
                assignments: vec![],
                expression: Rc::new(Expression::Cons(
                    Rc::new(Expression::Ident(0)),
                    Rc::new(Expression::Ident(1)),
                )),
            }),
        }),
    );
    test_one_line_expression(
        vec![Token::Left],
        Expression::Value(Value::Function {
            params: vec![0],
            scope: Box::new(Scope::Pure {
                assignments: vec![],
                expression: Rc::new(Expression::Left(Rc::new(Expression::Ident(0)))),
            }),
        }),
    );
    test_one_line_expression(
        vec![Token::Right],
        Expression::Value(Value::Function {
            params: vec![0],
            scope: Box::new(Scope::Pure {
                assignments: vec![],
                expression: Rc::new(Expression::Right(Rc::new(Expression::Ident(0)))),
            }),
        }),
    );
    test_one_line_expression(
        vec![Token::Empty],
        Expression::Value(Value::Function {
            params: vec![0],
            scope: Box::new(Scope::Pure {
                assignments: vec![],
                expression: Rc::new(Expression::Empty(Rc::new(Expression::Ident(0)))),
            }),
        }),
    );
    test_one_line_expression(
        vec![Token::Print],
        Expression::Value(Value::Function {
            params: vec![0],
            scope: Box::new(Scope::Impure {
                lines: vec![ImpureLine::Expression(Rc::new(Expression::PrintCall {
                    expr: Rc::new(Expression::Ident(0)),
                    newline: false,
                }))],
            }),
        }),
    );
    test_one_line_expression(
        vec![Token::Println],
        Expression::Value(Value::Function {
            params: vec![0],
            scope: Box::new(Scope::Impure {
                lines: vec![ImpureLine::Expression(Rc::new(Expression::PrintCall {
                    expr: Rc::new(Expression::Ident(0)),
                    newline: true,
                }))],
            }),
        }),
    );
    test_one_line_expression(
        vec![Token::Read],
        Expression::Value(Value::Function {
            params: vec![],
            scope: Box::new(Scope::Impure {
                lines: vec![ImpureLine::Assignment(0, Rc::new(Expression::ReadCall)),
                ImpureLine::Expression(Rc::new(Expression::Ident(0)))],
            }),
        }),
    );
}

#[test]
fn test_expression() {
    test_one_line_expression(
        vec![
            Token::True,
            Token::LeftBracket,
            Token::Number(String::from("1.5")),
            Token::Comma,
            Token::Number(String::from("2.5")),
            Token::RightBracket,
        ],
        Expression::FunctionCall {
            expr: Rc::new(Expression::Value(Value::Boolean(true))),
            args: vec![
                Rc::new(Expression::Value(Value::Number(Number::Float(1.5)))),
                Rc::new(Expression::Value(Value::Number(Number::Float(2.5)))),
            ],
        },
    );
}

#[test]
fn test_operation() {
    // cons(true, false) + cons(false, true)
    test_one_line_expression(
        vec![
            Token::Cons,
            Token::LeftBracket,
            Token::True,
            Token::Comma,
            Token::False,
            Token::RightBracket,
            Token::Operation(Op::Plus),
            Token::Cons,
            Token::LeftBracket,
            Token::False,
            Token::Comma,
            Token::True,
            Token::RightBracket,
        ],
        Expression::BinaryOperation(
            BinaryOp::Arithmetic(ArithBinOp::Plus),
            Rc::new(Expression::Cons(
                Rc::new(Expression::Value(Value::Boolean(true))),
                Rc::new(Expression::Value(Value::Boolean(false))),
            )),
            Rc::new(Expression::Cons(
                Rc::new(Expression::Value(Value::Boolean(false))),
                Rc::new(Expression::Value(Value::Boolean(true))),
            )),
        ),
    );
}

#[test]
fn test_multiple_operation() {
    // true + left(false) < false
    test_one_line_expression(
        vec![
            Token::True,
            Token::Operation(Op::Plus),
            Token::Left,
            Token::LeftBracket,
            Token::False,
            Token::RightBracket,
            Token::Operation(Op::Lt),
            Token::False,
        ],
        Expression::BinaryOperation(
            BinaryOp::Compare(CmpBinOp::Lt),
            Rc::new(Expression::BinaryOperation(
                BinaryOp::Arithmetic(ArithBinOp::Plus),
                Rc::new(Expression::Value(Value::Boolean(true))),
                Rc::new(Expression::Left(Rc::new(Expression::Value(
                    Value::Boolean(false),
                )))),
            )),
            Rc::new(Expression::Value(Value::Boolean(false))),
        ),
    );
}

#[test]
fn test_multiple_operation_or() {
    // (false == 0) || empty(true)
    test_one_line_expression(
        vec![
            Token::LeftBracket,
            Token::False,
            Token::Operation(Op::Eq),
            Token::Number(String::from("0")),
            Token::RightBracket,
            Token::Operation(Op::Or),
            Token::Empty,
            Token::LeftBracket,
            Token::True,
            Token::RightBracket,
        ],
        Expression::BinaryOperation(
            BinaryOp::Boolean(BoolBinOp::Or),
            Rc::new(Expression::BinaryOperation(
                BinaryOp::Compare(CmpBinOp::Eq),
                Rc::new(Expression::Value(Value::Boolean(false))),
                Rc::new(Expression::Value(Value::Number(Number::Integer(0)))),
            )),
            Rc::new(Expression::Empty(Rc::new(Expression::Value(
                Value::Boolean(true),
            )))),
        ),
    );
}

#[test]
fn test_brackets_expression() {
    // (true + cons(false, false)) >= true & (false | true)
    test_one_line_expression(
        vec![
            Token::LeftBracket,
            Token::True,
            Token::Operation(Op::Plus),
            Token::Cons,
            Token::LeftBracket,
            Token::False,
            Token::Comma,
            Token::False,
            Token::RightBracket,
            Token::RightBracket,
            Token::Operation(Op::GEq),
            Token::True,
            Token::Operation(Op::And),
            Token::LeftBracket,
            Token::False,
            Token::Operation(Op::Or),
            Token::True,
            Token::RightBracket,
        ],
        Expression::BinaryOperation(
            BinaryOp::Boolean(BoolBinOp::And),
            Rc::new(Expression::BinaryOperation(
                BinaryOp::Compare(CmpBinOp::GEq),
                Rc::new(Expression::BinaryOperation(
                    BinaryOp::Arithmetic(ArithBinOp::Plus),
                    Rc::new(Expression::Value(Value::Boolean(true))),
                    Rc::new(Expression::Cons(
                        Rc::new(Expression::Value(Value::Boolean(false))),
                        Rc::new(Expression::Value(Value::Boolean(false))),
                    )),
                )),
                Rc::new(Expression::Value(Value::Boolean(true))),
            )),
            Rc::new(Expression::BinaryOperation(
                BinaryOp::Boolean(BoolBinOp::Or),
                Rc::new(Expression::Value(Value::Boolean(false))),
                Rc::new(Expression::Value(Value::Boolean(true))),
            )),
        ),
    );
}

#[test]
fn test_nested_expression() {
    // cons(cons(12, true), false())
    test_one_line_expression(
        vec![
            Token::Cons,
            Token::LeftBracket,
            Token::Cons,
            Token::LeftBracket,
            Token::Number(String::from("12")),
            Token::Comma,
            Token::True,
            Token::RightBracket,
            Token::Comma,
            Token::False,
            Token::LeftBracket,
            Token::RightBracket,
            Token::RightBracket,
        ],
        Expression::Cons(
            Rc::new(Expression::Cons(
                Rc::new(Expression::Value(Value::Number(Number::Integer(12)))),
                Rc::new(Expression::Value(Value::Boolean(true))),
            )),
            Rc::new(Expression::FunctionCall {
                expr: Rc::new(Expression::Value(Value::Boolean(false))),
                args: vec![],
            }),
        ),
    );
}

#[test]
fn test_assignments_name() {
    let lines: Vec<Result<Line, std::io::Error>> = vec![
        Ok(Line {
            number: 1,
            indentation: 0,
            tokens: vec![
                Token::Name(String::from("foo")),
                Token::Assignment,
                Token::False,
            ],
        }),
        Ok(Line {
            number: 2,
            indentation: 0,
            tokens: vec![
                Token::Name(String::from("bar")),
                Token::Assignment,
                Token::True,
            ],
        }),
        Ok(Line {
            number: 3,
            indentation: 0,
            tokens: vec![Token::True],
        }),
    ];
    let result = Parser::new(Box::new(lines.into_iter()), PathBuf::new())
        .parse_outside_scope()
        .unwrap();
    assert_eq!(
        result,
        Scope::Impure {
            lines: vec![
                ImpureLine::Assignment(0, Rc::new(Expression::Value(Value::Boolean(false)))),
                ImpureLine::Assignment(1, Rc::new(Expression::Value(Value::Boolean(true)))),
                ImpureLine::Expression(Rc::new(Expression::Value(Value::Boolean(true))))
            ]
        }
    );
}

#[test]
fn test_function_multiple_args() {
    let lines: Vec<Result<Line, std::io::Error>> = vec![
        Ok(Line {
            number: 1,
            indentation: 0,
            tokens: vec![
                Token::Name(String::from("foo")),
                Token::Assignment,
                Token::LeftBoxBracket,
                Token::Name(String::from("a")),
                Token::Comma,
                Token::Name(String::from("b")),
                Token::RightBoxBracket,
                Token::Arrow,
            ],
        }),
        Ok(Line {
            number: 2,
            indentation: 1,
            tokens: vec![
                Token::Name(String::from("a")), // shadowing of a
                Token::Assignment,
                Token::Number(String::from("123")),
            ],
        }),
        Ok(Line {
            number: 2,
            indentation: 1,
            tokens: vec![Token::True],
        }),
        Ok(Line {
            number: 3,
            indentation: 0,
            tokens: vec![Token::True],
        }),
    ];
    let result = Parser::new(Box::new(lines.into_iter()), PathBuf::new())
        .parse_outside_scope()
        .unwrap();
    assert_eq!(
        result,
        Scope::Impure {
            lines: vec![
                ImpureLine::Assignment(
                    0,
                    Rc::new(Expression::Value(Value::Function {
                        params: vec![1, 2],
                        scope: Box::new(Scope::Pure {
                            assignments: vec![(
                                1,
                                Rc::new(Expression::Value(Value::Number(Number::Integer(123))))
                            )],
                            expression: Rc::new(Expression::Value(Value::Boolean(true)))
                        })
                    }))
                ),
                ImpureLine::Expression(Rc::new(Expression::Value(Value::Boolean(true))))
            ]
        }
    );
}

#[test]
fn test_function_no_args() {
    let lines: Vec<Result<Line, std::io::Error>> = vec![
        Ok(Line {
            number: 1,
            indentation: 0,
            tokens: vec![
                Token::Name(String::from("foo")),
                Token::Assignment,
                Token::LeftBoxBracket,
                Token::RightBoxBracket,
                Token::Arrow,
            ],
        }),
        Ok(Line {
            number: 3,
            indentation: 1,
            tokens: vec![Token::True],
        }),
        Ok(Line {
            number: 4,
            indentation: 0,
            tokens: vec![Token::True],
        }),
    ];
    let result = Parser::new(Box::new(lines.into_iter()), PathBuf::new())
        .parse_outside_scope()
        .unwrap();
    assert_eq!(
        result,
        Scope::Impure {
            lines: vec![
                ImpureLine::Assignment(
                    0,
                    Rc::new(Expression::Value(Value::Function {
                        params: vec![],
                        scope: Box::new(Scope::Pure {
                            assignments: vec![],
                            expression: Rc::new(Expression::Value(Value::Boolean(true)))
                        })
                    }))
                ),
                ImpureLine::Expression(Rc::new(Expression::Value(Value::Boolean(true))))
            ]
        }
    );
}

#[test]
fn test_function_impure_no_args() {
    let lines: Vec<Result<Line, std::io::Error>> = vec![
        Ok(Line {
            number: 1,
            indentation: 0,
            tokens: vec![
                Token::Name(String::from("foo")),
                Token::Assignment,
                Token::Impure,
                Token::Arrow,
            ],
        }),
        Ok(Line {
            number: 3,
            indentation: 1,
            tokens: vec![Token::True],
        }),
        Ok(Line {
            number: 3,
            indentation: 1,
            tokens: vec![Token::False],
        }),
        Ok(Line {
            number: 4,
            indentation: 0,
            tokens: vec![Token::True],
        }),
    ];
    let result = Parser::new(Box::new(lines.into_iter()), PathBuf::new())
        .parse_outside_scope()
        .unwrap();
    assert_eq!(
        result,
        Scope::Impure {
            lines: vec![
                ImpureLine::Assignment(
                    0,
                    Rc::new(Expression::Value(Value::Function {
                        params: vec![],
                        scope: Box::new(Scope::Impure {
                            lines: vec![
                                ImpureLine::Expression(Rc::new(Expression::Value(Value::Boolean(
                                    true
                                )))),
                                ImpureLine::Expression(Rc::new(Expression::Value(Value::Boolean(
                                    false
                                )))),
                            ]
                        })
                    }))
                ),
                ImpureLine::Expression(Rc::new(Expression::Value(Value::Boolean(true))))
            ]
        }
    );
}

#[test]
fn test_if_else() {
    let lines: Vec<Result<Line, std::io::Error>> = vec![
        Ok(Line {
            number: 1,
            indentation: 0,
            tokens: vec![
                Token::Name(String::from("foo")),
                Token::Assignment,
                Token::If,
                Token::False,
                Token::Then,
            ],
        }),
        Ok(Line {
            number: 2,
            indentation: 1,
            tokens: vec![
                Token::Name(String::from("foo")),
                Token::Assignment,
                Token::True,
            ],
        }),
        Ok(Line {
            number: 3,
            indentation: 1,
            tokens: vec![Token::Name(String::from("foo"))],
        }),
        Ok(Line {
            number: 4,
            indentation: 0,
            tokens: vec![Token::Else],
        }),
        Ok(Line {
            number: 5,
            indentation: 1,
            tokens: vec![Token::Number(String::from("12.3"))],
        }),
        Ok(Line {
            number: 6,
            indentation: 0,
            tokens: vec![Token::True],
        }),
    ];
    let result = Parser::new(Box::new(lines.into_iter()), PathBuf::new())
        .parse_outside_scope()
        .unwrap();
    assert_eq!(
        result,
        Scope::Impure {
            lines: vec![
                ImpureLine::Assignment(
                    0,
                    Rc::new(Expression::If {
                        condition: Rc::new(Expression::Value(Value::Boolean(false))),
                        then_scope: Box::new(Scope::Impure {
                            lines: vec![
                                ImpureLine::Assignment(
                                    0,
                                    Rc::new(Expression::Value(Value::Boolean(true)))
                                ),
                                ImpureLine::Expression(Rc::new(Expression::Ident(0)))
                            ]
                        }),
                        else_scope: Box::new(Scope::Impure {
                            lines: vec![ImpureLine::Expression(Rc::new(Expression::Value(
                                Value::Number(Number::Float(12.3))
                            )))]
                        })
                    })
                ),
                ImpureLine::Expression(Rc::new(Expression::Value(Value::Boolean(true))))
            ]
        }
    );
}

#[test]
fn test_if_else_in_pure_scope() {
    let lines: Vec<Result<Line, std::io::Error>> = vec![
        Ok(Line {
            number: 1,
            indentation: 0,
            tokens: vec![
                Token::Name(String::from("foo")),
                Token::Assignment,
                Token::Arrow,
            ],
        }),
        Ok(Line {
            number: 2,
            indentation: 1,
            tokens: vec![Token::If, Token::False, Token::Then, Token::True],
        }),
        Ok(Line {
            number: 4,
            indentation: 1,
            tokens: vec![Token::Else],
        }),
        Ok(Line {
            number: 5,
            indentation: 2,
            tokens: vec![Token::Number(String::from("12.3"))],
        }),
    ];
    let result = Parser::new(Box::new(lines.into_iter()), PathBuf::new())
        .parse_outside_scope()
        .unwrap();
    assert_eq!(
        result,
        Scope::Impure {
            lines: vec![ImpureLine::Assignment(
                0,
                Rc::new(Expression::Value(Value::Function {
                    params: vec![],
                    scope: Box::new(Scope::Pure {
                        assignments: vec![],
                        expression: Rc::new(Expression::If {
                            condition: Rc::new(Expression::Value(Value::Boolean(false))),
                            then_scope: Box::new(Scope::Pure {
                                assignments: vec![],
                                expression: Rc::new(Expression::Value(Value::Boolean(true)))
                            }),
                            else_scope: Box::new(Scope::Pure {
                                assignments: vec![],
                                expression: Rc::new(Expression::Value(Value::Number(
                                    Number::Float(12.3)
                                )))
                            })
                        })
                    })
                }))
            )]
        }
    );
}

#[test]
fn test_one_line_function_def() {
    test_one_line_expression(
        vec![
            Token::LeftBoxBracket,
            Token::Name(String::from("a")),
            Token::RightBoxBracket,
            Token::Arrow,
            Token::Name(String::from("a")),
        ],
        Expression::Value(Value::Function {
            params: vec![0],
            scope: Box::new(Scope::Pure {
                assignments: vec![],
                expression: Rc::new(Expression::Ident(0)),
            }),
        }),
    )
}

#[test]
fn test_one_line_if_else() {
    test_one_line_expression(
        vec![
            Token::If,
            Token::True,
            Token::Then,
            Token::True,
            Token::Else,
            Token::False,
        ],
        Expression::If {
            condition: Rc::new(Expression::Value(Value::Boolean(true))),
            then_scope: Box::new(Scope::Impure {
                lines: vec![ImpureLine::Expression(Rc::new(Expression::Value(
                    Value::Boolean(true),
                )))],
            }),
            else_scope: Box::new(Scope::Impure {
                lines: vec![ImpureLine::Expression(Rc::new(Expression::Value(
                    Value::Boolean(false),
                )))],
            }),
        },
    )
}
