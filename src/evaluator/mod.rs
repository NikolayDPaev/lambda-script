mod operations;
use by_address::ByAddress;
use rpds::HashTrieMap;
use std::{collections::HashMap, rc::Rc};

use crate::evaluator::operations::*;
use crate::parser::enums::*;

#[derive(Debug)]
pub enum EvaluatorError {
    UnknownName(Rc<Expression>),
    FunctionExpected(Rc<Expression>),
    ArgsAndParamsLensMismatch(Rc<Expression>),
    InvalidOperation {
        msg: String,
        expr: Rc<Expression>,
    },
    SideEffectInPureScope(Rc<Expression>),
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
}

pub struct Evaluator {
    memoization_map: HashMap<ByAddress<Rc<Expression>>, Rc<Expression>>,
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

macro_rules! make_thunk {
    ($expr:expr, $assignments: expr, $memoize: expr) => {
        Rc::new(Expression::Thunk(
            $expr.clone(),
            $assignments.clone(),
            $memoize,
        ))
    };
}

impl Evaluator {
    pub fn new() -> Evaluator {
        return Evaluator {
            memoization_map: HashMap::new(),
        };
    }

    pub fn eval_outside_scope(&mut self, scope: &Scope) -> Result<Value, EvaluatorError> {
        let mut expression = self.eval_scope(scope, HashTrieMap::new())?;
        loop {
            if let Expression::Value(value) = expression.as_ref() {
                return Ok(value.clone());
            }

            expression = self.eval_expression(expression, HashTrieMap::new(), false)?;
        }
    }

    fn end_eval(
        &mut self,
        expr: Rc<Expression>,
        assignments: HashTrieMap<String, Rc<Expression>>,
        memoize: bool,
    ) -> Result<Value, EvaluatorError> {
        let mut expression = expr;
        loop {
            if let Expression::Value(value) = expression.as_ref() {
                return Ok(value.clone());
            }
            //println!("Evaluating expression: {:?}", expression);
            expression = self.eval_expression(expression, assignments.clone(), memoize)?
        }
    }

    pub fn eval_scope(
        &mut self,
        scope: &Scope,
        outside_assignments: HashTrieMap<String, Rc<Expression>>,
    ) -> Result<Rc<Expression>, EvaluatorError> {
        match scope {
            Scope::Pure {
                assignments,
                expression,
            } => {
                let assignments_map = add_outside_assignments!(outside_assignments, assignments);

                Ok(make_thunk!(expression.clone(), assignments_map, true))
            }
            Scope::NonPure {
                assignments,
                statements,
            } => {
                let assignments_map = add_outside_assignments!(outside_assignments, assignments);
                let mut last_expr = Rc::new(Expression::Value(Value::Nil));
                for expr in statements {
                    last_expr = Rc::new(Expression::Value(self.end_eval(
                        expr.clone(),
                        assignments_map.clone(),
                        false,
                    )?));
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
        if let Some(expression) = self.memoization_map.get(&ByAddress(expr.clone())) {
            println!("using memoization for: {:?}", expression);
            return Ok(expression.clone());
        } else {
            let result = match expr.as_ref() {
                Expression::Value(_) => expr.clone(),
                Expression::Thunk(expr, env, memoize) => {
                    let result = self.eval_expression(expr.clone(), env.clone(), *memoize)?;
                    if *memoize {
                        //self.memoization_map.insert(ByAddress(expr.clone()), result.clone());
                    }
                    result
                }
                Expression::Name(string) => {
                    if let Some(expr) = assignments.get(string) {
                        make_thunk!(expr.clone(), assignments, memoize)
                    } else {
                        return Err(EvaluatorError::UnknownName(expr));
                    }
                }
                Expression::FunctionCall { name, args } => {
                    match self.end_eval(name.clone(), assignments.clone(), memoize)? {
                        Value::Function { params, scope } => {
                            if memoize && matches!(scope.as_ref(), Scope::NonPure { .. }) {
                                return Err(EvaluatorError::SideEffectInPureScope(name.clone()));
                            }
                            if args.len() != params.len() {
                                return Err(EvaluatorError::ArgsAndParamsLensMismatch(
                                    name.clone(),
                                ));
                            }
                            let assignments = params.into_iter().zip(args.into_iter()).fold(
                                assignments.clone(),
                                |acc, (string, expr)| {
                                    acc.insert(
                                        string.clone(),
                                        make_thunk!(
                                            expr,
                                            assignments,
                                            matches!(scope.as_ref(), Scope::NonPure { .. })
                                        ),
                                    )
                                },
                            );
                            self.eval_scope(&*scope, assignments)?
                        }
                        _ => {
                            return Err(EvaluatorError::FunctionExpected(name.clone()));
                        }
                    }
                }
                Expression::ReadCall => {
                    if memoize {
                        return Err(EvaluatorError::SideEffectInPureScope(expr.clone()));
                    }
                    todo!()
                }
                Expression::PrintCall(expr) => {
                    let value = self.end_eval(expr.clone(), assignments, memoize)?;
                    print(&value);
                    println!();
                    Rc::new(Expression::Value(Value::Nil))
                }
                Expression::Cons(left, right) => match (left.as_ref(), right.as_ref()) {
                    (Expression::Value(left), Expression::Value(right)) => {
                        Rc::new(Expression::Value(Value::Tuple(
                            Box::new(left.clone()),
                            Box::new(right.clone()),
                        )))
                    }
                    (Expression::Value(_), Expression::Thunk(..)) => Rc::new(Expression::Cons(
                        left.clone(),
                        self.eval_expression(right.clone(), assignments, memoize)?,
                    )),
                    (Expression::Thunk(..), Expression::Value(_)) => Rc::new(Expression::Cons(
                        self.eval_expression(left.clone(), assignments, memoize)?,
                        right.clone(),
                    )),
                    (Expression::Thunk(..), Expression::Thunk(..)) => Rc::new(Expression::Cons(
                        self.eval_expression(left.clone(), assignments.clone(), memoize)?,
                        self.eval_expression(right.clone(), assignments, memoize)?,
                    )),
                    (Expression::Value(_), _) => Rc::new(Expression::Cons(
                        left.clone(),
                        make_thunk!(right.clone(), assignments, memoize),
                    )),
                    (_, Expression::Value(_)) => Rc::new(Expression::Cons(
                        make_thunk!(left.clone(), assignments, memoize),
                        right.clone(),
                    )),
                    (_, _) => Rc::new(Expression::Cons(
                        make_thunk!(left.clone(), assignments.clone(), memoize),
                        make_thunk!(right.clone(), assignments, memoize),
                    )),
                },
                Expression::Left(inside_expr) => match inside_expr.as_ref() {
                    Expression::Cons(left, _) => left.clone(),
                    Expression::Value(Value::Tuple(left, _)) => {
                        Rc::new(Expression::Value(*left.clone()))
                    }
                    Expression::Value(_) => {
                        return Err(EvaluatorError::InvalidOperation {
                            msg: String::from("Left is defined only for cons and tuple"),
                            expr: expr.clone(),
                        })
                    }
                    _ => Rc::new(Expression::Left(self.eval_expression(
                        inside_expr.clone(),
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
                        return Err(EvaluatorError::InvalidOperation {
                            msg: String::from("Right is defined only for cons and tuple"),
                            expr: expr.clone(),
                        })
                    }
                    _ => Rc::new(Expression::Right(self.eval_expression(
                        inside_expr.clone(),
                        assignments,
                        memoize,
                    )?)),
                },
                Expression::Empty(inside_expr) => match inside_expr.as_ref() {
                    Expression::Value(Value::Nil) => Rc::new(Expression::Value(Value::Boolean(true))),
                    Expression::Cons(_, _) => Rc::new(Expression::Value(Value::Boolean(false))),
                    Expression::Value(Value::Tuple(_, _)) => Rc::new(Expression::Value(Value::Boolean(false))),
                    Expression::Value(_) => {
                        return Err(EvaluatorError::InvalidOperation {
                            msg: String::from("Empty is defined only for cons and tuple"),
                            expr: expr.clone(),
                        })
                    }
                    _ => Rc::new(Expression::Empty(self.eval_expression(
                        inside_expr.clone(),
                        assignments,
                        memoize,
                    )?)),
                },
                Expression::UnaryOperation(op, inside_expr) => {
                    Rc::new(Expression::Value(eval_unary_op(*op, &self.end_eval(inside_expr.clone(), assignments.clone(), memoize)?)?))
                },
                
                // match inside_expr.as_ref() {
                //     Expression::Value(value) => {
                //         Rc::new(Expression::Value(eval_unary_op(*op, value)?))
                //     }
                //     _ => Rc::new(Expression::UnaryOperation(
                //         *op,
                //         self.eval_expression(inside_expr.clone(), assignments, memoize)?,
                //     )),
                // },
                Expression::BinaryOperation(op, left, right) => {
                    // if matches!(op, BinaryOp::Compare(CmpBinOp::Eq)) {
                    //     match (left.as_ref(), right.as_ref()) {
                    //         (Expression::Cons(..), Expression::Value(Value::Nil)) => return Ok(Rc::new(Expression::Value(Value::Boolean(false)))),
                    //         (Expression::Thunk(_, _, _), Expression::Value(Value::Nil)) => return Ok(Rc::new(Expression::BinaryOperation(*op, self.eval_expression(left.clone(), assignments.clone(), memoize)?, right.clone()))),
                    //         _ => ()
                    //     }
                    // }
                    Rc::new(Expression::Value(eval_bin_op(
                        *op,
                        &self.end_eval(left.clone(), assignments.clone(), memoize)?,
                        &self.end_eval(right.clone(), assignments.clone(), memoize)?,
                    )?))
                }
                // match (left.as_ref(), right.as_ref()) {
                //     (Expression::Value(left), Expression::Value(right)) => {
                //         Rc::new(Expression::Value(eval_bin_op(*op, left, right)?))
                //     }
                //     (Expression::Value(_), _) => Rc::new(Expression::BinaryOperation(
                //         *op,
                //         left.clone(),
                //         self.eval_expression(right.clone(), assignments, memoize)?,
                //     )),
                //     (_, Expression::Value(_)) => Rc::new(Expression::BinaryOperation(
                //         *op,
                //         self.eval_expression(left.clone(), assignments, memoize)?,
                //         right.clone(),
                //     )),
                //     (_, _) => Rc::new(Expression::BinaryOperation(
                //         *op,
                //         self.eval_expression(left.clone(), assignments.clone(), memoize)?,
                //         self.eval_expression(right.clone(), assignments, memoize)?,
                //     )),
                // },
                Expression::If {
                    condition,
                    then_scope,
                    else_scope,
                } => match self.end_eval(condition.clone(), assignments.clone(), memoize)? {
                    Value::Boolean(true) => {
                        self.eval_scope(&**then_scope, assignments)?
                    }
                    Value::Boolean(false) => {
                        self.eval_scope(&**else_scope, assignments)?
                    }
                    _ => {
                        return Err(EvaluatorError::ConditionShouldEvaluateToBoolean(
                            expr.clone(),
                        ))
                    }
                },
            };
            Ok(result)
        }
    }
}
