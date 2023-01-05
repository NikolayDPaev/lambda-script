use by_address::ByAddress;
use rpds::HashTrieMap;
use std::{collections::HashMap, rc::Rc};

use crate::parser::enums::*;

pub enum EvaluatorError {
    UnknownName(Rc<Expression>),
    FunctionExpected(Rc<Expression>),
    ArgsAndParamsLensMismatch(Rc<Expression>),
    CallingNonPureFunctionInPureScope(Rc<Expression>),
    UndefinedOperation { msg: String, expr: Rc<Expression> },
}

pub struct Evaluator {
    memoization_map: HashMap<ByAddress<Rc<Expression>>, Value>,
}

impl Evaluator {
    pub fn eval_scope(
        &mut self,
        scope: &Scope,
        outside_assignments: HashTrieMap<String, Rc<Expression>>,
        memoize: bool,
    ) -> Result<Rc<Expression>, EvaluatorError> {
        let assignments = scope
            .assignments
            .iter()
            .fold(outside_assignments.clone(), |acc, (key, value)| {
                acc.insert(key.clone(), value.clone())
            });

        self.eval_expression(scope.expression.clone(), assignments, memoize)
    }

    fn eval_expression(
        &mut self,
        expr: Rc<Expression>,
        assignments: HashTrieMap<String, Rc<Expression>>,
        memoize: bool,
    ) -> Result<Rc<Expression>, EvaluatorError> {
        if let Some(value) = self.memoization_map.get(&ByAddress(expr.clone())) {
            return Ok(Rc::new(Expression::Value(value.clone())));
        } else {
            Ok(match expr.as_ref() {
                Expression::Value(v) => {
                    if memoize {
                        self.memoization_map
                            .insert(ByAddress(expr.clone()), v.clone());
                    }
                    expr.clone()
                }
                Expression::Name(string) => {
                    if let Some(expr) = assignments.get(string) {
                        self.eval_expression(expr.clone(), assignments, memoize)?
                    } else {
                        return Err(EvaluatorError::UnknownName(expr));
                    }
                }
                Expression::FunctionCall { name, args } => {
                    let evaluated_name =
                        self.eval_expression(name.clone(), assignments.clone(), memoize)?;
                    match evaluated_name.as_ref() {
                        Expression::Value(Value::Function {
                            pure,
                            params,
                            scope,
                        }) => {
                            if args.len() != params.len() {
                                return Err(EvaluatorError::ArgsAndParamsLensMismatch(
                                    evaluated_name.clone(),
                                ));
                            }
                            if memoize && !pure {
                                return Err(EvaluatorError::CallingNonPureFunctionInPureScope(
                                    evaluated_name.clone(),
                                ));
                            }
                            let assignments = params
                                .into_iter()
                                .zip(args.into_iter())
                                .fold(assignments, |acc, (string, expr)| {
                                    acc.insert(string.clone(), expr.clone())
                                });
                            self.eval_scope(&*scope, assignments, *pure)?
                        }
                        Expression::Value(_) => {
                            return Err(EvaluatorError::FunctionExpected(evaluated_name.clone()));
                        }
                        _ => Rc::new(Expression::FunctionCall {
                            name: evaluated_name,
                            args: args.clone(),
                        }),
                    }
                }
                Expression::ReadCall => todo!(),
                Expression::PrintCall(expr) => {
                    match expr.as_ref() {
                        Expression::Value(value) => todo!(),
                        _ => Rc::new(Expression::PrintCall(self.eval_expression(expr.clone(), assignments, memoize)?))
                    }
                },
                Expression::Cons(left, right) => match (left.as_ref(), right.as_ref()) {
                    (Expression::Value(left), Expression::Value(right)) => {
                        Rc::new(Expression::Value(Value::Tuple(
                            Box::new(left.clone()),
                            Box::new(right.clone()),
                        )))
                    }
                    (Expression::Value(_), _) => Rc::new(Expression::Cons(
                        left.clone(),
                        self.eval_expression(right.clone(), assignments, memoize)?,
                    )),
                    (_, Expression::Value(_)) => Rc::new(Expression::Cons(
                        self.eval_expression(right.clone(), assignments, memoize)?,
                        left.clone(),
                    )),
                    (_, _) => Rc::new(Expression::Cons(
                        self.eval_expression(left.clone(), assignments.clone(), memoize)?,
                        self.eval_expression(right.clone(), assignments, memoize)?,
                    )),
                },
                Expression::Left(inside_expr) => match inside_expr.as_ref() {
                    Expression::Cons(left, _) => left.clone(),
                    Expression::Value(Value::Tuple(left, _)) => {
                        Rc::new(Expression::Value(*left.clone()))
                    }
                    Expression::Value(_) => {
                        return Err(EvaluatorError::UndefinedOperation {
                            msg: String::from("Left is defined only for cons and tuple"),
                            expr: expr.clone(),
                        })
                    }
                    _ => Rc::new(Expression::Left(self.eval_expression(
                        expr,
                        assignments,
                        memoize,
                    )?)),
                },
                Expression::Right(inside_expr) => match inside_expr.as_ref() {
                    Expression::Cons(_, right) => right.clone(),
                    Expression::Value(Value::Tuple(_, right)) => {
                        Rc::new(Expression::Value(*right.clone()))
                    }
                    Expression::Value(_) => {
                        return Err(EvaluatorError::UndefinedOperation {
                            msg: String::from("Right is defined only for cons and tuple"),
                            expr: expr.clone(),
                        })
                    }
                    _ => Rc::new(Expression::Right(self.eval_expression(
                        expr,
                        assignments,
                        memoize,
                    )?)),
                },
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
