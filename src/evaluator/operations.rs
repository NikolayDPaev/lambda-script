use std::{
    io::{BufWriter, Write},
    rc::Rc,
};

use crate::parser::enums::{
    ArithBinOp, BinaryOp, BoolBinOp, CmpBinOp, Expression, Number, UnaryOp, Value,
};

use super::EvaluatorError;

fn try_string(value: &Value) -> Option<String> {
    let mut string = String::new();
    let mut value = value;
    loop {
        match value {
            Value::Tuple(left, right) => {
                match (left.as_ref(), right.as_ref()) {
                    (Value::Char(ch), Value::Tuple(..)) => {
                        string.push(*ch);
                        //string.insert(0, *ch);
                        value = right;
                    }
                    (Value::Char(ch), Value::Nil) => {
                        string.push(*ch);
                        //string.insert(0, *ch);
                        return Some(string);
                    }
                    _ => {
                        return None;
                    }
                }
            }
            _ => {
                return None;
            }
        }
    }
}

pub fn print<W: Write>(value: &Value, output: &mut BufWriter<W>) {
    match value {
        Value::Boolean(true) => write!(output, "true").unwrap(),
        Value::Boolean(false) => write!(output, "false").unwrap(),
        Value::Nil => write!(output, "nil").unwrap(),
        Value::Number(Number::Float(f)) => write!(output, "{}", f).unwrap(),
        Value::Number(Number::Integer(i)) => write!(output, "{}", i).unwrap(),
        Value::Char(char) => write!(output, "{}", char).unwrap(),
        Value::Tuple(left, right) => {
            if let Some(string) = try_string(value) {
                write!(output, "{}", string).unwrap();
                return;
            }
            write!(output, "(").unwrap();
            print(left, output);
            write!(output, ", ").unwrap();
            print(right, output);
            write!(output, ")").unwrap();
        }
        Value::Function { params, .. } => write!(output, "Function[{}]", params.len()).unwrap(),
    }
}

pub fn eval_unary_op(op: UnaryOp, value: &Value) -> Result<Value, EvaluatorError> {
    match op {
        UnaryOp::Minus => match value {
            Value::Number(Number::Float(f)) => Ok(Value::Number(Number::Float(-f))),
            Value::Number(Number::Integer(i)) => Ok(Value::Number(Number::Integer(-i))),
            _ => Err(EvaluatorError::InvalidUnaryOperation {
                msg: String::from("Unary operation minus is defined only for numbers"),
                expr: Rc::new(Expression::Value(value.clone())),
            }),
        },
        UnaryOp::Negation => match value {
            Value::Boolean(true) => Ok(Value::Boolean(false)),
            Value::Boolean(false) => Ok(Value::Boolean(true)),
            _ => Err(EvaluatorError::InvalidUnaryOperation {
                msg: String::from("Unary operation negation is defined only for booleans"),
                expr: Rc::new(Expression::Value(value.clone())),
            }),
        },
    }
}

fn eval_bool_op(op: BoolBinOp, left: &Value, right: &Value) -> Result<Value, EvaluatorError> {
    match op {
        BoolBinOp::And => match (left, right) {
            (Value::Boolean(a), Value::Boolean(b)) => return Ok(Value::Boolean(*a && *b)),
            _ => Err(EvaluatorError::BooleanError {
                op,
                value_1: left.clone(),
                value_2: right.clone(),
            }),
        },
        BoolBinOp::Or => match (left, right) {
            (Value::Boolean(a), Value::Boolean(b)) => return Ok(Value::Boolean(*a || *b)),
            _ => Err(EvaluatorError::BooleanError {
                op,
                value_1: left.clone(),
                value_2: right.clone(),
            }),
        },
        BoolBinOp::Xor => match (left, right) {
            (Value::Boolean(a), Value::Boolean(b)) => return Ok(Value::Boolean(*a ^ *b)),
            _ => Err(EvaluatorError::BooleanError {
                op,
                value_1: left.clone(),
                value_2: right.clone(),
            }),
        },
    }
}

macro_rules! cmp_error {
    ($op:expr, $value_1:expr, $value_2:expr) => {
        Err(EvaluatorError::ComparisonError {
            op: $op,
            value_1: $value_1.clone(),
            value_2: $value_2.clone(),
        })
    };
}

fn eval_cmp_op(op: CmpBinOp, left: &Value, right: &Value) -> Result<Value, EvaluatorError> {
    match op {
        CmpBinOp::Eq => match (left, right) {
            (Value::Boolean(a), Value::Boolean(b)) => Ok(Value::Boolean(*a == *b)),
            (Value::Nil, Value::Nil) => Ok(Value::Boolean(true)),
            (_, Value::Nil) => Ok(Value::Boolean(false)),
            (Value::Nil, _) => Ok(Value::Boolean(false)),
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(*a == *b)),
            (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(*a == *b)),
            (Value::Char(a), Value::Number(b)) => {
                Ok(Value::Boolean(Number::Integer(*a as i32) == *b))
            }
            (Value::Number(a), Value::Char(b)) => {
                Ok(Value::Boolean(*a == Number::Integer(*b as i32)))
            }
            (Value::Tuple(a1, b1), Value::Tuple(a2, b2)) => {
                let left = eval_cmp_op(op, a1.as_ref(), a2.as_ref())?;
                let right = eval_cmp_op(op, b1.as_ref(), b2.as_ref())?;
                eval_bool_op(BoolBinOp::And, &left, &right)
            }
            (Value::Boolean(_), _) => cmp_error!(op, left.clone(), right.clone()),
            (Value::Number(_), _) => cmp_error!(op, left.clone(), right.clone()),
            (Value::Char(_), _) => cmp_error!(op, left.clone(), right.clone()),
            (Value::Tuple(..), ..) => cmp_error!(op, left.clone(), right.clone()),
            (Value::Function { .. }, ..) => cmp_error!(op, left.clone(), right.clone()),
        },
        CmpBinOp::NEq => {
            let eq = eval_cmp_op(CmpBinOp::Eq, left, right).map_err(|_| {
                EvaluatorError::ComparisonError {
                    op,
                    value_1: left.clone(),
                    value_2: right.clone(),
                }
            })?;
            eval_unary_op(UnaryOp::Negation, &eq)
        }
        CmpBinOp::Lt => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(*a < *b)),
            (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(*a < *b)),
            (Value::Char(a), Value::Number(b)) => {
                Ok(Value::Boolean(Number::Integer(*a as i32) < *b))
            }
            (Value::Number(a), Value::Char(b)) => {
                Ok(Value::Boolean(*a < Number::Integer(*b as i32)))
            }
            (..) => cmp_error!(op, left.clone(), right.clone()),
        },
        CmpBinOp::Gt => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(*a > *b)),
            (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(*a > *b)),
            (Value::Char(a), Value::Number(b)) => {
                Ok(Value::Boolean(Number::Integer(*a as i32) > *b))
            }
            (Value::Number(a), Value::Char(b)) => {
                Ok(Value::Boolean(*a > Number::Integer(*b as i32)))
            }
            (..) => cmp_error!(op, left.clone(), right.clone()),
        },
        CmpBinOp::LEq => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(*a <= *b)),
            (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(*a <= *b)),
            (Value::Char(a), Value::Number(b)) => {
                Ok(Value::Boolean(Number::Integer(*a as i32) <= *b))
            }
            (Value::Number(a), Value::Char(b)) => {
                Ok(Value::Boolean(*a <= Number::Integer(*b as i32)))
            }
            (..) => cmp_error!(op, left.clone(), right.clone()),
        },
        CmpBinOp::GEq => match (left, right) {
            (Value::Number(a), Value::Number(b)) => Ok(Value::Boolean(*a >= *b)),
            (Value::Char(a), Value::Number(b)) => {
                Ok(Value::Boolean(Number::Integer(*a as i32) >= *b))
            }
            (Value::Number(a), Value::Char(b)) => {
                Ok(Value::Boolean(*a >= Number::Integer(*b as i32)))
            }
            (Value::Char(a), Value::Char(b)) => Ok(Value::Boolean(*a >= *b)),
            (..) => cmp_error!(op, left.clone(), right.clone()),
        },
    }
}

macro_rules! arith_error {
    ($op:expr, $value_1:expr, $value_2:expr) => {
        Err(EvaluatorError::ArithmeticError {
            op: $op,
            value_1: $value_1.clone(),
            value_2: $value_2.clone(),
        })
    };
}

fn eval_arith_op(op: ArithBinOp, left: &Value, right: &Value) -> Result<Value, EvaluatorError> {
    match op {
        ArithBinOp::Plus => match (left, right) {
            (Value::Number(Number::Integer(a)), Value::Number(Number::Integer(b))) => {
                Ok(Value::Number(Number::Integer(a + b)))
            }
            (Value::Number(Number::Integer(a)), Value::Number(Number::Float(b))) => {
                Ok(Value::Number(Number::Float(*a as f64 + b)))
            }
            (Value::Number(Number::Float(a)), Value::Number(Number::Integer(b))) => {
                Ok(Value::Number(Number::Float(a + *b as f64)))
            }
            (Value::Number(Number::Float(a)), Value::Number(Number::Float(b))) => {
                Ok(Value::Number(Number::Float(a + b)))
            }
            (Value::Char(c_1), Value::Char(c_2)) => {
                Ok(Value::Number(Number::Integer(*c_1 as i32 + *c_2 as i32)))
            }
            (Value::Number(Number::Integer(n)), Value::Char(c)) => {
                Ok(Value::Number(Number::Integer(n + *c as i32)))
            }
            (Value::Char(c), Value::Number(Number::Integer(n))) => Ok(Value::Char(
                char::from_u32((*c as i32 + n) as u32).ok_or(EvaluatorError::ArithmeticError {
                    op,
                    value_1: left.clone(),
                    value_2: right.clone(),
                })?,
            )),
            (..) => arith_error!(op, left.clone(), right.clone()),
        },
        ArithBinOp::Minus => match (left, right) {
            (Value::Number(_), Value::Number(_)) => {
                let neg_right = eval_unary_op(UnaryOp::Minus, right)?;
                eval_arith_op(ArithBinOp::Plus, left, &neg_right)
            }
            (Value::Char(c_1), Value::Char(c_2)) => {
                Ok(Value::Number(Number::Integer(*c_1 as i32 - *c_2 as i32)))
            }
            (Value::Number(Number::Integer(n)), Value::Char(c)) => {
                Ok(Value::Number(Number::Integer(n - *c as i32)))
            }
            (Value::Char(c), Value::Number(Number::Integer(n))) => Ok(Value::Char(
                char::from_u32((*c as i32 - n) as u32).ok_or(EvaluatorError::ArithmeticError {
                    op,
                    value_1: left.clone(),
                    value_2: right.clone(),
                })?,
            )),
            (..) => arith_error!(op, left.clone(), right.clone()),
        },
        ArithBinOp::Division => match (left, right) {
            (Value::Number(Number::Integer(a)), Value::Number(Number::Integer(b))) => {
                if a % b == 0 {
                    Ok(Value::Number(Number::Integer(a / b)))
                } else {
                    Ok(Value::Number(Number::Float(*a as f64 / *b as f64)))
                }
            }
            (Value::Number(Number::Integer(a)), Value::Number(Number::Float(b))) => {
                Ok(Value::Number(Number::Float(*a as f64 / b)))
            }
            (Value::Number(Number::Float(a)), Value::Number(Number::Integer(b))) => {
                Ok(Value::Number(Number::Float(a / *b as f64)))
            }
            (Value::Number(Number::Float(a)), Value::Number(Number::Float(b))) => {
                Ok(Value::Number(Number::Float(a / b)))
            }
            (..) => arith_error!(op, left.clone(), right.clone()),
        },
        ArithBinOp::IntDivision => match (left, right) {
            (Value::Number(Number::Integer(a)), Value::Number(Number::Integer(b))) => {
                Ok(Value::Number(Number::Integer(a / b)))
            }
            (Value::Number(Number::Integer(a)), Value::Number(Number::Float(b))) => {
                Ok(Value::Number(Number::Integer(a / *b as i32)))
            }
            (Value::Number(Number::Float(a)), Value::Number(Number::Integer(b))) => {
                Ok(Value::Number(Number::Integer(*a as i32 / b)))
            }
            (Value::Number(Number::Float(a)), Value::Number(Number::Float(b))) => {
                Ok(Value::Number(Number::Integer(*a as i32 / *b as i32)))
            }
            (..) => arith_error!(op, left.clone(), right.clone()),
        },
        ArithBinOp::Multiplication => match (left, right) {
            (Value::Number(Number::Integer(a)), Value::Number(Number::Integer(b))) => {
                Ok(Value::Number(Number::Integer(a * b)))
            }
            (Value::Number(Number::Integer(a)), Value::Number(Number::Float(b))) => {
                Ok(Value::Number(Number::Float(*a as f64 * b)))
            }
            (Value::Number(Number::Float(a)), Value::Number(Number::Integer(b))) => {
                Ok(Value::Number(Number::Float(a * *b as f64)))
            }
            (Value::Number(Number::Float(a)), Value::Number(Number::Float(b))) => {
                Ok(Value::Number(Number::Float(a * b)))
            }
            (..) => arith_error!(op, left.clone(), right.clone()),
        },
        ArithBinOp::Exponentiation => match (left, right) {
            (Value::Number(Number::Integer(a)), Value::Number(Number::Integer(b))) => {
                if *b > 0 {
                    Ok(Value::Number(Number::Integer(a.pow(*b as u32))))
                } else {
                    Ok(Value::Number(Number::Float((*a as f64).powi(*b))))
                }
            }
            (Value::Number(Number::Integer(a)), Value::Number(Number::Float(b))) => {
                Ok(Value::Number(Number::Float((*a as f64).powf(*b))))
            }
            (Value::Number(Number::Float(a)), Value::Number(Number::Integer(b))) => {
                Ok(Value::Number(Number::Float(a.powi(*b))))
            }
            (Value::Number(Number::Float(a)), Value::Number(Number::Float(b))) => {
                Ok(Value::Number(Number::Float(a.powf(*b))))
            }
            (..) => arith_error!(op, left.clone(), right.clone()),
        },
        ArithBinOp::Modulo => match (left, right) {
            (Value::Number(Number::Integer(a)), Value::Number(Number::Integer(b))) => {
                Ok(Value::Number(Number::Integer(a % b)))
            }
            (Value::Number(Number::Float(a)), Value::Number(Number::Integer(b))) => {
                Ok(Value::Number(Number::Integer((*a as i32) % b)))
            }
            (..) => arith_error!(op, left.clone(), right.clone()),
        },
    }
}

pub fn eval_bin_op(op: BinaryOp, left: &Value, right: &Value) -> Result<Value, EvaluatorError> {
    match op {
        BinaryOp::Boolean(bool_op) => eval_bool_op(bool_op, left, right),
        BinaryOp::Compare(cmp_op) => eval_cmp_op(cmp_op, left, right),
        BinaryOp::Arithmetic(num_op) => eval_arith_op(num_op, left, right),
    }
}
