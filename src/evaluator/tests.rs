use super::*;

fn test_expression(e: Expression, v: Value) {
    let inner_input = vec![];
    let mut input: BufReader<_> = BufReader::new(inner_input.as_slice());
    let mut output = BufWriter::new(vec![]);
    let mut evaluator = Evaluator::new(&mut input, &mut output, false);
    assert_eq!(
        evaluator
            .force_eval(Rc::new(e), HashTrieMap::new(), true)
            .unwrap(),
        v
    )
}

macro_rules! test_arithmetic_num_expression {
    ($n1: expr, $op: expr, $n2: expr, $r: expr) => {
        test_expression(
            Expression::BinaryOperation(
                BinaryOp::Arithmetic($op),
                Rc::new(Expression::Value(Value::Number($n1))),
                Rc::new(Expression::Value(Value::Number($n2))),
            ),
            Value::Number($r),
        );
    };
}

#[test]
fn test_evaluate_arithmetic_operations() {
    test_arithmetic_num_expression!(
        Number::Integer(13),
        ArithBinOp::Modulo,
        Number::Integer(2),
        Number::Integer(1)
    );
    test_arithmetic_num_expression!(
        Number::Integer(13),
        ArithBinOp::IntDivision,
        Number::Integer(2),
        Number::Integer(6)
    );
    test_arithmetic_num_expression!(
        Number::Integer(13),
        ArithBinOp::Division,
        Number::Integer(2),
        Number::Float(6.5)
    );
    test_arithmetic_num_expression!(
        Number::Float(13.5),
        ArithBinOp::Plus,
        Number::Integer(2),
        Number::Float(15.5)
    );
    test_arithmetic_num_expression!(
        Number::Float(13.5),
        ArithBinOp::Plus,
        Number::Float(2.5),
        Number::Float(16.0)
    );
    test_arithmetic_num_expression!(
        Number::Float(13.5),
        ArithBinOp::Minus,
        Number::Float(2.5),
        Number::Float(11.0)
    );
}

macro_rules! test_cmp_num_expression {
    ($n1: expr, $op: expr, $n2: expr, $r: expr) => {
        test_expression(
            Expression::BinaryOperation(
                BinaryOp::Compare($op),
                Rc::new(Expression::Value(Value::Number($n1))),
                Rc::new(Expression::Value(Value::Number($n2))),
            ),
            Value::Boolean($r),
        );
    };
}

#[test]
fn test_evaluate_cmp_num_operations() {
    test_cmp_num_expression!(Number::Integer(13), CmpBinOp::Eq, Number::Integer(2), false);
    test_cmp_num_expression!(Number::Integer(13), CmpBinOp::Eq, Number::Integer(13), true);
    test_cmp_num_expression!(Number::Integer(1), CmpBinOp::Eq, Number::Integer(10), false);
    test_cmp_num_expression!(
        Number::Integer(13),
        CmpBinOp::NEq,
        Number::Integer(13),
        false
    );
    test_cmp_num_expression!(Number::Integer(13), CmpBinOp::NEq, Number::Integer(2), true);
    test_cmp_num_expression!(Number::Float(2.0), CmpBinOp::Eq, Number::Integer(2), true);
    test_cmp_num_expression!(Number::Float(2.0), CmpBinOp::GEq, Number::Integer(2), true);
    test_cmp_num_expression!(Number::Float(2.1), CmpBinOp::Gt, Number::Integer(2), true);
    test_cmp_num_expression!(Number::Integer(2), CmpBinOp::Gt, Number::Integer(2), false);
    test_cmp_num_expression!(Number::Float(13.5), CmpBinOp::Eq, Number::Float(2.5), false);
}

macro_rules! test_cmp_eq_expression {
    ($n1: expr, $n2: expr, $r: expr) => {
        test_expression(
            Expression::BinaryOperation(
                BinaryOp::Compare(CmpBinOp::Eq),
                Rc::new(Expression::Value($n1)),
                Rc::new(Expression::Value($n2)),
            ),
            Value::Boolean($r),
        );
    };
}

#[test]
fn test_eq() {
    test_cmp_eq_expression!(Value::Char('A'), Value::Number(Number::Integer(65)), true);
    test_cmp_eq_expression!(Value::Char('A'), Value::Char('A'), true);
    test_cmp_eq_expression!(Value::Char('A'), Value::Nil, false);
    test_cmp_eq_expression!(Value::Number(Number::Integer(0)), Value::Nil, false);
    test_cmp_eq_expression!(Value::Nil, Value::Nil, true);
    test_cmp_eq_expression!(Value::Boolean(true), Value::Boolean(true), true);
    test_cmp_eq_expression!(Value::Boolean(false), Value::Boolean(true), false);
}

macro_rules! test_bool_expression {
    ($n1: expr, $op: expr, $n2: expr, $r: expr) => {
        test_expression(
            Expression::BinaryOperation(
                BinaryOp::Boolean($op),
                Rc::new(Expression::Value($n1)),
                Rc::new(Expression::Value($n2)),
            ),
            Value::Boolean($r),
        );
    };
}

#[test]
fn test_bool() {
    test_bool_expression!(
        Value::Boolean(true),
        BoolBinOp::And,
        Value::Boolean(true),
        true
    );
    test_bool_expression!(
        Value::Boolean(true),
        BoolBinOp::And,
        Value::Boolean(false),
        false
    );
    test_bool_expression!(
        Value::Boolean(false),
        BoolBinOp::Or,
        Value::Boolean(true),
        true
    );
    test_bool_expression!(
        Value::Boolean(true),
        BoolBinOp::Or,
        Value::Boolean(false),
        true
    );
    test_bool_expression!(
        Value::Boolean(true),
        BoolBinOp::Xor,
        Value::Boolean(false),
        true
    );
    test_bool_expression!(
        Value::Boolean(false),
        BoolBinOp::Xor,
        Value::Boolean(false),
        false
    );
    test_bool_expression!(
        Value::Boolean(false),
        BoolBinOp::Or,
        Value::Boolean(false),
        false
    );
    test_bool_expression!(
        Value::Boolean(false),
        BoolBinOp::And,
        Value::Boolean(false),
        false
    );
}

macro_rules! test_arithmetic_expression {
    ($n1: expr, $op: expr, $n2: expr, $r: expr) => {
        test_expression(
            Expression::BinaryOperation(
                BinaryOp::Arithmetic($op),
                Rc::new(Expression::Value($n1)),
                Rc::new(Expression::Value($n2)),
            ),
            $r,
        );
    };
}

#[test]
fn test_evaluate_char_operations() {
    test_arithmetic_expression!(
        Value::Char('a'),
        ArithBinOp::Plus,
        Value::Number(Number::Integer(2)),
        Value::Char('c')
    );
    test_arithmetic_expression!(
        Value::Char('c'),
        ArithBinOp::Minus,
        Value::Char('a'),
        Value::Number(Number::Integer(2))
    );
    test_arithmetic_expression!(
        Value::Char('c'),
        ArithBinOp::Minus,
        Value::Number(Number::Integer(2)),
        Value::Char('a')
    );
}

#[test]
fn test_evaluate_cons_operations() {
    test_expression(
        Expression::Cons(
            Rc::new(Expression::Value(Value::Boolean(true))),
            Rc::new(Expression::Value(Value::Char('a'))),
        ),
        Value::Tuple(Box::new(Value::Boolean(true)), Box::new(Value::Char('a'))),
    );
    test_expression(
        Expression::Left(Rc::new(Expression::Cons(
            Rc::new(Expression::Value(Value::Boolean(true))),
            Rc::new(Expression::Value(Value::Char('a'))),
        ))),
        Value::Boolean(true),
    );
    test_expression(
        Expression::Right(Rc::new(Expression::Cons(
            Rc::new(Expression::Value(Value::Boolean(true))),
            Rc::new(Expression::Value(Value::Char('a'))),
        ))),
        Value::Char('a'),
    );
    test_expression(
        Expression::Empty(Rc::new(Expression::Cons(
            Rc::new(Expression::Value(Value::Boolean(true))),
            Rc::new(Expression::Value(Value::Char('a'))),
        ))),
        Value::Boolean(false),
    );
    test_expression(
        Expression::Empty(Rc::new(Expression::Value(Value::Nil))),
        Value::Boolean(true),
    );
}

#[test]
fn test_evaluate_if_else() {
    test_expression(
        Expression::If {
            condition: Rc::new(Expression::Value(Value::Boolean(true))),
            then_scope: Box::new(Scope::Pure {
                assignments: vec![],
                expression: Rc::new(Expression::Value(Value::Boolean(false))),
            }),
            else_scope: Box::new(Scope::Pure {
                assignments: vec![],
                expression: Rc::new(Expression::Value(Value::Boolean(true))),
            }),
        },
        Value::Boolean(false),
    );
    test_expression(
        Expression::If {
            condition: Rc::new(Expression::Value(Value::Boolean(false))),
            then_scope: Box::new(Scope::Pure {
                assignments: vec![],
                expression: Rc::new(Expression::Value(Value::Boolean(false))),
            }),
            else_scope: Box::new(Scope::Pure {
                assignments: vec![],
                expression: Rc::new(Expression::Value(Value::Boolean(true))),
            }),
        },
        Value::Boolean(true),
    )
}

#[test]
fn test_evaluate_pure_function() {
    let inner_input = vec![];
    let mut input: BufReader<_> = BufReader::new(inner_input.as_slice());
    let mut output = BufWriter::new(vec![]);
    let mut evaluator = Evaluator::new(&mut input, &mut output, false);
    assert_eq!(
        evaluator
            .force_eval(
                Rc::new(Expression::FunctionCall {
                    expr: Rc::new(Expression::Ident(0)),
                    args: vec![Rc::new(Expression::Value(Value::Boolean(true)))]
                }),
                HashTrieMap::new().insert(
                    0,
                    Rc::new(Expression::Value(Value::Function {
                        params: vec![1],
                        scope: Box::new(Scope::Pure {
                            assignments: vec![],
                            expression: Rc::new(Expression::UnaryOperation(
                                UnaryOp::Negation,
                                Rc::new(Expression::Ident(1))
                            ))
                        })
                    }))
                ),
                true
            )
            .unwrap(),
        Value::Boolean(false)
    )
}

#[test]
fn test_evaluate_read_print_function() {
    let input = String::from("bcd");
    let mut output = Vec::<u8>::new();
    let mut input_stream: BufReader<_> = BufReader::new(input.as_bytes());
    let mut output_stream = BufWriter::new(&mut output);
    let mut evaluator = Evaluator::new(&mut input_stream, &mut output_stream, false);
    assert_eq!(
        evaluator
            .force_eval(
                Rc::new(Expression::FunctionCall {
                    expr: Rc::new(Expression::Ident(0)),
                    args: vec![]
                }),
                HashTrieMap::new().insert(
                    0,
                    Rc::new(Expression::Value(Value::Function {
                        params: vec![],
                        scope: Box::new(Scope::Impure {
                            lines: vec![
                                ImpureLine::Assignment(1, Rc::new(Expression::ReadCall)),
                                ImpureLine::Expression(Rc::new(Expression::PrintCall {
                                    expr: Rc::new(Expression::Ident(1)),
                                    newline: true
                                })),
                                ImpureLine::Expression(Rc::new(Expression::PrintCall {
                                    expr: Rc::new(Expression::Cons(
                                        Rc::new(Expression::Value(Value::Char('a'))),
                                        Rc::new(Expression::Ident(1))
                                    )),
                                    newline: false
                                }))
                            ]
                        })
                    }))
                ),
                false
            )
            .unwrap(),
        Value::Nil
    );
    std::mem::drop(output_stream);
    assert_eq!(
        String::from_utf8(output).unwrap(),
        String::from("bcd\nabcd")
    );
}
