#[cfg(test)]
use crate::lexer::{Line, Token};
use crate::parser::*;

fn test_one_line_expression(tokens: Vec<Token>, expression: Expression) {
    let lines: Vec<Result<Line, std::io::Error>> = vec![Ok(Line {
        number: 1,
        indentation: 0,
        tokens,
    })];
    let result = parse(Box::new(lines.into_iter())).unwrap();
    assert_eq!(
        result,
        Scope {
            assignments: vec![],
            expression
        }
    );
}

fn test_one_line_expression_error(tokens: Vec<Token>, error: ParserError) {
    let lines: Vec<Result<Line, std::io::Error>> = vec![Ok(Line {
        number: 1,
        indentation: 0,
        tokens,
    })];
    let result = parse(Box::new(lines.into_iter()));
    assert_eq!(result, Err(error));
}

#[test]
fn test_values() {
    test_one_line_expression(
        vec![Token::Name(String::from("foo"))],
        Expression::Name(String::from("foo")),
    );
    test_one_line_expression(
        vec![Token::Name(String::from("foo"))],
        Expression::Name(String::from("foo")),
    );
    test_one_line_expression(
        vec![Token::Str(String::from("bar"))],
        Expression::Cons(
            Box::new(Expression::Value(Value::Char('b'))),
            Box::new(Expression::Cons(
                Box::new(Expression::Value(Value::Char('a'))),
                Box::new(Expression::Cons(
                    Box::new(Expression::Value(Value::Char('r'))),
                    Box::new(Expression::Value(Value::Nil)),
                )),
            )),
        ),
    );
    test_one_line_expression(
        vec![Token::Number(String::from("123"))],
        Expression::Value(Value::Number(Number::Integer(123))),
    );
    test_one_line_expression_error(
        vec![Token::Number(String::from("1a23"))],
        ParserError::NumberParseError { line: 1 },
    );
    test_one_line_expression(
        vec![Token::Number(String::from("123.5"))],
        Expression::Value(Value::Number(Number::Float(123.5))),
    );
    test_one_line_expression_error(
        vec![Token::Number(String::from("1.a23"))],
        ParserError::NumberParseError { line: 1 },
    );
    test_one_line_expression(
        vec![Token::Char(String::from("a"))],
        Expression::Value(Value::Char('a')),
    );
    test_one_line_expression_error(
        vec![Token::Char(String::from("abc"))],
        ParserError::CharParseError { line: 1 },
    );
    test_one_line_expression_error(
        vec![Token::Char(String::from(""))],
        ParserError::CharParseError { line: 1 },
    );
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
            Token::Name(String::from("foo")),
            Token::Comma,
            Token::Name(String::from("bar")),
            Token::RightBracket,
        ],
        Expression::Cons(
            Box::new(Expression::Name(String::from("foo"))),
            Box::new(Expression::Name(String::from("bar"))),
        ),
    );

    test_one_line_expression_error(
        vec![
            Token::Cons,
            Token::LeftBracket,
            Token::Name(String::from("foo")),
            Token::RightBracket,
        ],
        ParserError::CommaExpected { line: 1 },
    );

    test_one_line_expression(
        vec![
            Token::Print,
            Token::LeftBracket,
            Token::Name(String::from("foo")),
            Token::RightBracket,
        ],
        Expression::PrintCall(Box::new(Expression::Name(String::from("foo")))),
    );

    test_one_line_expression(
        vec![
            Token::Left,
            Token::LeftBracket,
            Token::Name(String::from("foo")),
            Token::RightBracket,
        ],
        Expression::Left(Box::new(Expression::Name(String::from("foo")))),
    );

    test_one_line_expression(
        vec![
            Token::Right,
            Token::LeftBracket,
            Token::Name(String::from("foo")),
            Token::RightBracket,
        ],
        Expression::Right(Box::new(Expression::Name(String::from("foo")))),
    );

    test_one_line_expression_error(
        vec![
            Token::Right,
        ],
        ParserError::LeftBracketExpected { line: 1 }
    );

    test_one_line_expression(
        vec![
            Token::Read,
        ],
        Expression::ReadCall,
    );


}

#[test]
fn test_expression() {
    test_one_line_expression(
        vec![
            Token::Name(String::from("foo")),
            Token::LeftBracket,
            Token::Name(String::from("a")),
            Token::Comma,
            Token::Name(String::from("b")),
            Token::RightBracket,
        ],
        Expression::FunctionCall {
            name: String::from("foo"),
            args: vec![
                Expression::Name(String::from("a")),
                Expression::Name(String::from("b")),
            ],
        },
    );
}

#[test]
fn test_operation() {
    // foo(a, b) + foo(b, c)
    test_one_line_expression(
        vec![
            Token::Name(String::from("foo")),
            Token::LeftBracket,
            Token::Name(String::from("a")),
            Token::Comma,
            Token::Name(String::from("b")),
            Token::RightBracket,
            Token::Operation(Op::Plus),
            Token::Name(String::from("foo")),
            Token::LeftBracket,
            Token::Name(String::from("b")),
            Token::Comma,
            Token::Name(String::from("c")),
            Token::RightBracket,
        ],
        Expression::BinaryOperation(
            BinaryOp::Number(NumberBinOp::Plus),
            Box::new(Expression::FunctionCall {
                name: String::from("foo"),
                args: vec![
                    Expression::Name(String::from("a")),
                    Expression::Name(String::from("b")),
                ],
            }),
            Box::new(Expression::FunctionCall {
                name: String::from("foo"),
                args: vec![
                    Expression::Name(String::from("b")),
                    Expression::Name(String::from("c")),
                ],
            }),
        ),
    );
}

#[test]
fn test_multiple_operation() {
    // a + foo(b, c) < b
    test_one_line_expression(
        vec![
            Token::Name(String::from("a")),
            Token::Operation(Op::Plus),
            Token::Name(String::from("foo")),
            Token::LeftBracket,
            Token::Name(String::from("b")),
            Token::Comma,
            Token::Name(String::from("c")),
            Token::RightBracket,
            Token::Operation(Op::Lt),
            Token::Name(String::from("b")),
        ],
        Expression::BinaryOperation(
            BinaryOp::Number(NumberBinOp::Plus),
            Box::new(Expression::Name(String::from("a"))),
            Box::new(Expression::BinaryOperation(
                BinaryOp::Compare(CmpBinOp::Lt),
                Box::new(Expression::FunctionCall {
                    name: String::from("foo"),
                    args: vec![
                        Expression::Name(String::from("b")),
                        Expression::Name(String::from("c")),
                    ],
                }),
                Box::new(Expression::Name(String::from("b"))),
            )),
        ),
    );
}

#[test]
fn test_brackets_expression() {
    // (a + foo(b, c)) >= b + (c - d)
    test_one_line_expression(
        vec![
            Token::LeftBracket,
            Token::Name(String::from("a")),
            Token::Operation(Op::Plus),
            Token::Name(String::from("foo")),
            Token::LeftBracket,
            Token::Name(String::from("b")),
            Token::Comma,
            Token::Name(String::from("c")),
            Token::RightBracket,
            Token::RightBracket,
            Token::Operation(Op::GEq),
            Token::Name(String::from("b")),
            Token::Operation(Op::Plus),
            Token::LeftBracket,
            Token::Name(String::from("c")),
            Token::Operation(Op::Minus),
            Token::Name(String::from("d")),
            Token::RightBracket,
        ],
        Expression::BinaryOperation(
            BinaryOp::Compare(CmpBinOp::GEq),
            Box::new(Expression::BinaryOperation(
                BinaryOp::Number(NumberBinOp::Plus),
                Box::new(Expression::Name(String::from("a"))),
                Box::new(Expression::FunctionCall {
                    name: String::from("foo"),
                    args: vec![
                        Expression::Name(String::from("b")),
                        Expression::Name(String::from("c")),
                    ],
                }),
            )),
            Box::new(Expression::BinaryOperation(
                BinaryOp::Number(NumberBinOp::Plus),
                Box::new(Expression::Name(String::from("b"))),
                Box::new(Expression::BinaryOperation(
                    BinaryOp::Number(NumberBinOp::Minus),
                    Box::new(Expression::Name(String::from("c"))),
                    Box::new(Expression::Name(String::from("d"))),
                )),
            )),
        ),
    );
}

#[test]
fn test_nested_expression() {
    // foo(a(12, b), c())
    test_one_line_expression(
        vec![
            Token::Name(String::from("foo")),
            Token::LeftBracket,
            Token::Name(String::from("a")),
            Token::LeftBracket,
            Token::Number(String::from("12")),
            Token::Comma,
            Token::Name(String::from("b")),
            Token::RightBracket,
            Token::Comma,
            Token::Name(String::from("c")),
            Token::LeftBracket,
            Token::RightBracket,
            Token::RightBracket,
        ],
        Expression::FunctionCall {
            name: String::from("foo"),
            args: vec![
                Expression::FunctionCall {
                    name: String::from("a"),
                    args: vec![
                        Expression::Value(Value::Number(Number::Integer(12))),
                        Expression::Name(String::from("b")),
                    ],
                },
                Expression::FunctionCall {
                    name: String::from("c"),
                    args: vec![],
                },
            ],
        },
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
                Token::Name(String::from("bar")),
            ],
        }),
        Ok(Line {
            number: 2,
            indentation: 0,
            tokens: vec![
                Token::Name(String::from("bar")),
                Token::Assignment,
                Token::Name(String::from("foo")),
            ],
        }),
        Ok(Line {
            number: 3,
            indentation: 0,
            tokens: vec![Token::Name(String::from("foo"))],
        }),
    ];
    let result = parse(Box::new(lines.into_iter())).unwrap();
    assert_eq!(
        result,
        Scope {
            assignments: vec![
                (String::from("foo"), Expression::Name(String::from("bar"))),
                (String::from("bar"), Expression::Name(String::from("foo")))
            ],
            expression: Expression::Name(String::from("foo"))
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
                Token::Name(String::from("bar")),
                Token::Assignment,
                Token::Number(String::from("123")),
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
            tokens: vec![Token::Name(String::from("foo"))],
        }),
    ];
    let result = parse(Box::new(lines.into_iter())).unwrap();
    assert_eq!(
        result,
        Scope {
            assignments: vec![(
                String::from("foo"),
                Expression::Value(Value::Function {
                    pure: true,
                    params: vec![String::from("a"), String::from("b")],
                    scope: Box::new(Scope {
                        assignments: vec![(
                            String::from("bar"),
                            Expression::Value(Value::Number(Number::Integer(123)))
                        )],
                        expression: Expression::Name(String::from("foo"))
                    })
                })
            )],
            expression: Expression::Name(String::from("foo"))
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
            tokens: vec![Token::Name(String::from("foo"))],
        }),
        Ok(Line {
            number: 4,
            indentation: 0,
            tokens: vec![Token::Name(String::from("foo"))],
        }),
    ];
    let result = parse(Box::new(lines.into_iter())).unwrap();
    assert_eq!(
        result,
        Scope {
            assignments: vec![(
                String::from("foo"),
                Expression::Value(Value::Function {
                    pure: true,
                    params: vec![],
                    scope: Box::new(Scope {
                        assignments: vec![],
                        expression: Expression::Name(String::from("foo"))
                    })
                })
            )],
            expression: Expression::Name(String::from("foo"))
        }
    );
}

#[test]
fn test_function_nonpure_no_args() {
    let lines: Vec<Result<Line, std::io::Error>> = vec![
        Ok(Line {
            number: 1,
            indentation: 0,
            tokens: vec![
                Token::Name(String::from("foo")),
                Token::Assignment,
                Token::NonPure,
                Token::Arrow,
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
            tokens: vec![Token::Name(String::from("foo"))],
        }),
    ];
    let result = parse(Box::new(lines.into_iter())).unwrap();
    assert_eq!(
        result,
        Scope {
            assignments: vec![(
                String::from("foo"),
                Expression::Value(Value::Function {
                    pure: false,
                    params: vec![],
                    scope: Box::new(Scope {
                        assignments: vec![],
                        expression: Expression::Name(String::from("foo"))
                    })
                })
            )],
            expression: Expression::Name(String::from("foo"))
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
                Token::Name(String::from("bar")),
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
            tokens: vec![Token::Name(String::from("foo"))],
        }),
    ];
    let result = parse(Box::new(lines.into_iter())).unwrap();
    assert_eq!(
        result,
        Scope {
            assignments: vec![(
                String::from("foo"),
                Expression::If {
                    condition: Box::new(Expression::Name(String::from("bar"))),
                    then_scope: Box::new(Scope {
                        assignments: vec![(
                            String::from("foo"),
                            Expression::Value(Value::Boolean(true))
                        )],
                        expression: Expression::Name(String::from("foo"))
                    }),
                    else_scope: Box::new(Scope {
                        assignments: vec![],
                        expression: Expression::Value(Value::Number(Number::Float(12.3)))
                    })
                }
            )],
            expression: Expression::Name(String::from("foo"))
        }
    );
}
