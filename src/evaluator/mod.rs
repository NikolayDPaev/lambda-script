use by_address::ByAddress;
use rpds::HashTrieMap;
use std::collections::HashMap;

use crate::parser::enums::*;

pub enum EvaluatorError {
    UnknownName(String),
    FunctionExpected(String),
    ArgsAndParamsLensMismatch(String),
    CallingNonPureFunctionInPureScope(String),
}

pub struct Evaluator<'a> {
    memoization_map: HashMap<ByAddress<&'a Expression>, Value>,
}

impl<'a> Evaluator<'a> {
    pub fn eval_scope(
        &mut self,
        scope: &'a Scope,
        outside_assignments: HashTrieMap<String, &'a Expression>,
        memoize: bool,
    ) -> Result<Expression, EvaluatorError> {
        let assignments = scope
            .assignments
            .iter()
            .fold(outside_assignments.clone(), |acc, (key, value)| {
                acc.insert(key.clone(), value)
            });

        self.eval_expression(&scope.expression, assignments, memoize)
    }

    fn eval_expression(
        &mut self,
        expr: &'a Expression,
        assignments: HashTrieMap<String, &'a Expression>,
        memoize: bool,
    ) -> Result<Expression, EvaluatorError> {
        if let Some(value) = self.memoization_map.get(&ByAddress(expr)) {
            return Ok(Expression::Value(value.clone()));
        } else {
            Ok (match expr {
                Expression::Value(v) => {
                    if memoize {
                        self.memoization_map.insert(ByAddress(expr), v.clone());
                    }
                    Expression::Value(v.clone())
                }
                Expression::Name(string) => {
                    if let Some(expr) = assignments.get(string) {
                        self.eval_expression(*expr, assignments, memoize)?
                    } else {
                        return Err(EvaluatorError::UnknownName(string.to_owned()));
                    }
                }
                Expression::FunctionCall { name, args } => {
                    if let Some(Expression::Value(Value::Function {
                        pure,
                        params,
                        scope,
                    })) = assignments.get(name)
                    {
                        if args.len() != params.len() {
                            return Err(EvaluatorError::ArgsAndParamsLensMismatch(name.to_owned()));
                        }
                        if memoize && !pure {
                            return Err(EvaluatorError::CallingNonPureFunctionInPureScope(
                                name.to_owned(),
                            ));
                        }
                        let assignments = params
                            .iter()
                            .zip(args.iter())
                            .fold(assignments.clone(), |acc, (string, expr)| {
                                acc.insert(string.to_string(), expr)
                            });
                        self.eval_scope(scope, assignments, *pure)?
                    } else if let Some(_) = assignments.get(name) {
                        return Err(EvaluatorError::FunctionExpected(name.to_owned()));
                    } else {
                        return Err(EvaluatorError::UnknownName(name.to_owned()));
                    }
                }
                Expression::ReadCall => todo!(),
                Expression::PrintCall(_) => todo!(),
                Expression::Cons(_, _) => todo!(),
                Expression::Left(expr) => {
                    let mut expr = **expr;
                    loop {
                        if let Expression::Cons(left, _) = expr {
                            break *left
                        }
                        let new_expr = self.eval_expression(&expr, assignments.clone(), memoize)?;
                        expr = new_expr;
                    }
                },
                Expression::Right(_) => todo!(),
                Expression::UnaryOperation(_, _) => todo!(),
                Expression::BinaryOperation(_, _, _) => todo!(),
                Expression::If {
                    condition,
                    then_scope,
                    else_scope,
                } => todo!(),
            })
        }
    }
}
