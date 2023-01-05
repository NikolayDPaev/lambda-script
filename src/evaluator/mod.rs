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
        scope: Scope,
        outside_assignments: HashTrieMap<String, Rc<Expression>>,
        memoize: bool,
    ) -> Result<Rc<Expression>, EvaluatorError> {
        let assignments = scope
            .assignments
            .into_iter()
            .fold(outside_assignments.clone(), |acc, (key, value)| {
                acc.insert(key.clone(), Rc::new(value))
            });

        self.eval_expression(Rc::new(scope.expression), assignments, memoize)
    }

    fn eval_expression(
        &mut self,
        expr: Rc<Expression>,
        assignments: HashTrieMap<String, Rc<Expression>>,
        memoize: bool,
    ) -> Result<Rc<Expression>, EvaluatorError> {
        if let Some(value) = self.memoization_map.get(&ByAddress(expr)) {
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
                        self.eval_expression(*expr, assignments, memoize)?
                    } else {
                        return Err(EvaluatorError::UnknownName(expr));
                    }
                }
                Expression::FunctionCall { name, args } => {
                    if let Some(rc_expr) = assignments.get(name) {
                        match **rc_expr {
                            Expression::Value(Value::Function {
                                pure,
                                params,
                                scope,
                            }) => {
                                if args.len() != params.len() {
                                    return Err(EvaluatorError::ArgsAndParamsLensMismatch(
                                        *rc_expr,
                                    ));
                                }
                                if memoize && !pure {
                                    return Err(EvaluatorError::CallingNonPureFunctionInPureScope(
                                        *rc_expr,
                                    ));
                                }
                                let assignments = params
                                    .into_iter()
                                    .zip(args.into_iter())
                                    .fold(assignments.clone(), |acc, (string, expr)| {
                                        acc.insert(string, Rc::new(*expr))
                                    });
                                self.eval_scope(*scope, assignments, pure)?
                            }
                            _ => {
                                return Err(EvaluatorError::FunctionExpected(*rc_expr));
                            }
                        }
                    } else {
                        return Err(EvaluatorError::UnknownName(expr));
                    }
                }
                Expression::ReadCall => todo!(),
                Expression::PrintCall(_) => todo!(),
                Expression::Cons(_, _) => todo!(),
                Expression::Left(inside_expr) => match *inside_expr {
                    Expression::Cons(left, _) => Rc::new(*left),
                    Expression::Value(Value::Tuple(left, _)) => Rc::new(Expression::Value(*left)),
                    Expression::Value(_) => {
                        return Err(EvaluatorError::UndefinedOperation {
                            msg: String::from("Left is defined only for cons and tuple"),
                            expr: expr.clone(),
                        })
                    }
                    _ => Expression::Left(Box::new(self.eval_expression(
                        expr,
                        assignments,
                        memoize,
                    )?)),
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
