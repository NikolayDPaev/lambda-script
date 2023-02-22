use std::rc::Rc;

use crate::parser::enums::{ArithBinOp, BoolBinOp, CmpBinOp, Expression, Value};

#[derive(Debug)]
pub enum EvaluatorError {
    //UnknownName(Rc<Expression>),
    FunctionExpected(Rc<Expression>),
    ArgsAndParamsLengthsMismatch(Rc<Expression>),
    InvalidUnaryOperation {
        msg: String,
        expr: Rc<Expression>,
    },
    SideEffectInPureScope(Rc<Expression>),
    UnexpectedRead(),
    ConditionShouldEvaluateToBoolean(Rc<Expression>),
    ComparisonError {
        op: CmpBinOp,
        value_1: Value,
        value_2: Value,
    },
    ArithmeticError {
        op: ArithBinOp,
        value_1: Value,
        value_2: Value,
    },
    BooleanError {
        op: BoolBinOp,
        value_1: Value,
        value_2: Value,
    },
}

pub fn process_evaluator_error(err: EvaluatorError) -> String {
    let error = match err {
        // EvaluatorError::UnknownName(expression) => {
        //     format!("Use of undeclared name in this scope:\n{:?}", expression)
        // }
        EvaluatorError::FunctionExpected(expression) => {
            format!("Expected function, actual expression is:\n{:?}", expression)
        }
        EvaluatorError::ArgsAndParamsLengthsMismatch(expression) => format!(
            "Wrong number of arguments provided to the functions:\n{:?}",
            expression
        ),
        EvaluatorError::InvalidUnaryOperation { msg, expr } => {
            format!("Invalid operation in the expression:\n{:?}\n{}", expr, msg)
        }
        EvaluatorError::SideEffectInPureScope(expr) => {
            format!("Expression produces side effect in pure scope:\n{:?}", expr)
        }
        EvaluatorError::ConditionShouldEvaluateToBoolean(expr) => {
            format!("Expression was expected to evaluate to boolean:\n{:?}", expr)
        }
        EvaluatorError::ComparisonError {
            op,
            value_1,
            value_2,
        } => format!(
            "{:?} and {:?} cannot be compared with '{:?}'",
            value_1, value_2, op
        ),
        EvaluatorError::ArithmeticError {
            op,
            value_1,
            value_2,
        } => format!(
            "Operation '{:?}' cannot be applied to {:?} and {:?}",
            op, value_1, value_2
        ),
        EvaluatorError::BooleanError {
            op,
            value_1,
            value_2,
        } => format!(
            "Operation '{:?}' cannot be applied to {:?} and {:?}",
            op, value_1, value_2
        ),
        EvaluatorError::UnexpectedRead() => format!(
            "Read call at invalid position. The only possible place for the read is as assignment expression in impure scope"
        ),
    };
    format!("Error: {}\n\nFor more info run with --debug.\n", error)
}
