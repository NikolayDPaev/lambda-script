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
    SideEffectInPureScope (Rc<Expression>),
    ConditionShouldEvaluateToBoolean(Rc<Expression>),
}

fn print(value: Value) {
    todo!()
}

fn eval_unary_op(op: UnaryOp, value: Value) -> Result<Value, EvaluatorError> {
    todo!()
}

fn eval_bin_op(op: BinaryOp, left: Value, right: Value) -> Result<Value, EvaluatorError> {
    todo!()
}

pub struct Evaluator {
    memoization_map: HashMap<ByAddress<Rc<Expression>>, Value>,
}

macro_rules! add_outside_assignments {
    ($outside_assignments: expr, $assignments: expr) => {
        $assignments
            .iter()
            .fold($outside_assignments.clone(), |acc, (key, value)| {
                acc.insert(key.clone(), value.clone())
            })    
    };
}


impl Evaluator {
    fn end_eval(&mut self,
        expr: Rc<Expression>,
        assignments: HashTrieMap<String, Rc<Expression>>,
        memoize: bool,
    ) -> Result<Value, EvaluatorError> {
        let mut expression = expr;
        loop {
            if let Expression::Value(value) = expression.as_ref() {
               return Ok(value.clone())
            }
            expression = self.eval_expression(expression, assignments.clone(), memoize)?
        }
    }
    pub fn eval_scope(
        &mut self,
        scope: &Scope,
        outside_assignments: HashTrieMap<String, Rc<Expression>>,
    ) -> Result<Rc<Expression>, EvaluatorError> {
        match scope {
            Scope::Pure { assignments, expression } => {
                let assignments_map = add_outside_assignments!(outside_assignments, assignments);
        
                self.eval_expression(expression.clone(), assignments_map, true)
            },
            Scope::NonPure { assignments, statements } => {
                let assignments_map = add_outside_assignments!(outside_assignments, assignments);
                let mut last_expr = Rc::new(Expression::Value(Value::Nil));
                for expr in statements {
                    last_expr = Rc::new(Expression::Value(self.end_eval(expr.clone(), assignments_map.clone(), false)?));
                }
                Ok(last_expr)                
            }
        }
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
                            params,
                            scope,
                        }) => {
                            if memoize && matches!(scope.as_ref(), Scope::NonPure { .. }) {
                                return Err(EvaluatorError::SideEffectInPureScope(evaluated_name.clone()))
                            }
                            if args.len() != params.len() {
                                return Err(EvaluatorError::ArgsAndParamsLensMismatch(
                                    evaluated_name.clone(),
                                ));
                            }
                            let assignments = params
                                .into_iter()
                                .zip(args.into_iter())
                                .fold(assignments, |acc, (string, expr)| {
                                    acc.insert(string.clone(), expr.clone())
                                });
                            self.eval_scope(&*scope, assignments)?
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
                Expression::ReadCall => {
                    if memoize {
                        return Err(EvaluatorError::SideEffectInPureScope(expr.clone()));
                    }
                    todo!()
                },
                Expression::PrintCall(expr) => {
                    let value = self.end_eval(expr.clone(), assignments, memoize)?;
                    print(value);
                    Rc::new(Expression::Value(Value::Nil))
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
                Expression::UnaryOperation(op, inside_expr) => match inside_expr.as_ref() {
                    Expression::Value(value) => {
                        Rc::new(Expression::Value(eval_unary_op(*op, value.clone())?))
                    },
                    _ => Rc::new(Expression::UnaryOperation(*op,
                        self.eval_expression(
                        expr,
                        assignments,
                        memoize,
                    )?)),
                },
                Expression::BinaryOperation(op, left, right) => match (left.as_ref(), right.as_ref()) {
                    (Expression::Value(left), Expression::Value(right)) => {
                        Rc::new(Expression::Value(eval_bin_op(*op, left.clone(), right.clone())?))
                    }
                    (Expression::Value(_), _) => Rc::new(Expression::BinaryOperation(
                        *op,
                        left.clone(),
                        self.eval_expression(right.clone(), assignments, memoize)?,
                    )),
                    (_, Expression::Value(_)) => Rc::new(Expression::BinaryOperation(
                        *op,
                        self.eval_expression(right.clone(), assignments, memoize)?,
                        left.clone(),
                    )),
                    (_, _) => Rc::new(Expression::BinaryOperation(
                        *op,
                        self.eval_expression(left.clone(), assignments.clone(), memoize)?,
                        self.eval_expression(right.clone(), assignments, memoize)?,
                    )),
                },
                Expression::If {
                    condition,
                    then_scope,
                    else_scope,
                } => {
                    match condition.as_ref() {
                        Expression::Value(Value::Boolean(true)) => {                            
                            self.eval_scope(&**then_scope, assignments)?
                        },
                        Expression::Value(Value::Boolean(false)) => {
                            self.eval_scope(&**else_scope, assignments)?
                        },
                        Expression::Value(_) => {
                            return Err(EvaluatorError::ConditionShouldEvaluateToBoolean(expr.clone()))
                        },
                        _ => Rc::new(Expression::If { condition: self.eval_expression(condition.clone(), assignments, memoize)?, then_scope: then_scope.clone(), else_scope: else_scope.clone() })
                    }
                },
            })
        }
    }
}
