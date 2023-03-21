use std::rc::Rc;

use crate::parser::enums::{
    display_expr, display_value, ArithBinOp, BoolBinOp, CmpBinOp, Expression, Value,
};

#[derive(Debug)]
pub enum EvaluatorError {
    //UnknownName(Rc<Expression>),
    ErrorWithInfo {
        expr: Rc<Expression>,
        error: Box<EvaluatorError>,
    },
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

fn format_error(err: EvaluatorError, names: &[String]) -> String {
    match err {
        EvaluatorError::FunctionExpected(expr) => {
            format!("Expected function, actual expression is:\n\t{}", display_expr(expr, names))
        }
        EvaluatorError::ArgsAndParamsLengthsMismatch(expr) => format!(
            "Wrong number of arguments provided to the function:\n\t{}",
            display_expr(expr, names)
        ),
        EvaluatorError::InvalidUnaryOperation { msg, expr } => {
            format!("Invalid operation in the expression:\n\t{}\n\n{}", display_expr(expr, names), msg)
        }
        EvaluatorError::SideEffectInPureScope(expr) => {
            format!("Expression produces side effect in pure scope:\n\t{}", display_expr(expr, names))
        }
        EvaluatorError::ConditionShouldEvaluateToBoolean(expr) => {
            format!("If condition should evaluate to boolean in:\n\t{}", display_expr(expr, names))
        }
        EvaluatorError::ComparisonError {
            op,
            value_1,
            value_2,
        } => format!(
            "{} and {} cannot be compared with '{:?}'",
            display_value(&value_1, names), display_value(&value_2, names), op
        ),
        EvaluatorError::ArithmeticError {
            op,
            value_1,
            value_2,
        } => format!(
            "Operation '{:?}' cannot be applied to {} and {}",
            op, display_value(&value_1, names), display_value(&value_2, names)
        ),
        EvaluatorError::BooleanError {
            op,
            value_1,
            value_2,
        } => format!(
            "Operation '{:?}' cannot be applied to {} and {}",
            op, display_value(&value_1, names), display_value(&value_2, names)
        ),
        EvaluatorError::UnexpectedRead() => format!(
            "Read call at invalid position. The only possible place for the read is in an assignment in impure scope"
        ),
        EvaluatorError::ErrorWithInfo { expr, error } => {
            format!(
                "While evaluating expression:\n\t{}\n{}", display_expr(expr, names), format_error(*error, names) 
            )
        },
    }
}

pub fn process_evaluator_error(err: EvaluatorError, names: &[String]) -> String {
    format!("Error:\n{}\n", format_error(err, names))
}
