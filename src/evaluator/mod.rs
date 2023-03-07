pub mod errors;
mod operations;
#[cfg(test)]
mod tests;

use rpds::HashTrieMap;
use std::io::{BufRead, BufReader, BufWriter, Read, Write};
use std::rc::Rc;
use std::cell::RefCell;

use crate::evaluator::errors::EvaluatorError;
use crate::evaluator::operations::*;
use crate::parser::enums::*;

macro_rules! add_outside_assignments {
    ($outside_assignments: expr, $assignments: expr) => {
        $assignments
            .iter()
            .fold($outside_assignments.clone(), |acc, (key, value)| {
                acc.insert(key.clone(), value.clone())
            })
    };
}

// Creates a new thunk if the expression is not already a thunk or value.
macro_rules! make_thunk {
    ($expr:expr, $assignments: expr, $pure: expr) => {
        match $expr.as_ref() {
            // If the expression is a function then in its body it might have names
            // referring to expressions in the context at declaration.
            // That is why we must preserve the context.
            Expression::Value(Value::Function { .. }) => Rc::new(Expression::Thunk(
                RefCell::new($expr.clone()),
                $assignments.clone(),
                $pure,
            )),
            // if the expression is thunk or value, it does not need its context
            Expression::Thunk(..) | Expression::Value(..) => $expr.clone(),
            _ => Rc::new(Expression::Thunk(
                RefCell::new($expr.clone()),
                $assignments.clone(),
                $pure,
            )),
        }
    };
}

pub struct Evaluator<'a, R: Read, W: Write> {
    input: &'a mut BufReader<R>,
    output: &'a mut BufWriter<W>,
}

impl<R, W> Evaluator<'_, R, W>
where
    R: Read,
    W: Write,
{
    pub fn new<'a>(
        input: &'a mut BufReader<R>,
        output: &'a mut BufWriter<W>,
    ) -> Evaluator<'a, R, W> {
        Evaluator {
            input,
            output,
        }
    }

    // entry point for the evaluator
    pub fn eval_outside_scope(&mut self, scope: &Scope) -> Result<Value, EvaluatorError> {
        let mut expression = self.eval_scope(scope, HashTrieMap::new())?;
        loop {
            if let Expression::Value(value) = expression.as_ref() {
                return Ok(value.clone());
            }
            expression = self.eval_expression(expression, HashTrieMap::new(), false)?;
        }
    }

    // Calls eval_expression on the expression in a loop until it is a value
    // if the expression is a thunk, memoizes the result
    fn force_eval(
        &mut self,
        expr: Rc<Expression>,
        assignments: HashTrieMap<u32, Rc<Expression>>,
        pure: bool,
    ) -> Result<Value, EvaluatorError> {
        let mut expression = expr.clone();
        let mut thunks_for_memo = vec![];
        loop {
            if pure && matches!(expression.as_ref(), Expression::Thunk(..)) {
                thunks_for_memo.push(expression.clone());
            }
            if let Expression::Value(value) = expression.as_ref() {
                while let Some(thunk_expr) = thunks_for_memo.pop() {
                    match thunk_expr.as_ref() {
                        Expression::Thunk(inside_refcell,_ , _ ) => {
                            inside_refcell.replace(expression.clone());
                        },
                        _ => unreachable!()
                    }
                }
                return Ok(value.clone());
            }
            expression = self
                .eval_expression(expression, assignments.clone(), pure)
                .map_err(|error| EvaluatorError::ErrorWithInfo {
                    expr: expr.clone(),
                    error: Box::new(error),
                })?
        }
    }

    // reads a line from stdin and returns the string
    pub fn force_read(&mut self) -> Rc<Expression> {
        let mut string = String::new();
        self.input.read_line(&mut string).unwrap();
        if string.ends_with('\n') {
            string.pop();
            if string.ends_with('\r') {
                string.pop();
            }
        }
        Rc::new(Expression::Value(crate::parser::parse_string(&string)))
    }

    // evaluates the scope
    // if it is impure, evaluates all expressions and assignments in order
    fn eval_scope(
        &mut self,
        scope: &Scope,
        outside_assignments: HashTrieMap<u32, Rc<Expression>>,
    ) -> Result<Rc<Expression>, EvaluatorError> {
        match scope {
            Scope::Pure {
                assignments,
                expression,
            } => {
                let assignments_map = add_outside_assignments!(outside_assignments, assignments);

                Ok(make_thunk!(expression.clone(), assignments_map, true))
            }
            Scope::Impure { lines } => {
                // add the outside assignments
                // force eval the assignments and the expressions
                // eval the read calls
                let mut assignments_map = outside_assignments.clone();
                let mut return_expr = Value::Nil;
                for line in lines {
                    match line {
                        ImpureLine::Assignment(ident, expr) => {
                            let evaluated_expr = match expr.as_ref() {
                                // if assignment expression is read call, force its evaluation
                                Expression::ReadCall => self.force_read(),
                                _ => Rc::new(Expression::Value(self.force_eval(
                                    expr.clone(),
                                    assignments_map.clone(),
                                    false,
                                )?)),
                            };
                            assignments_map = assignments_map.insert(*ident, evaluated_expr);
                            return_expr = Value::Nil;
                        }
                        ImpureLine::Expression(expr) => {
                            return_expr =
                                self.force_eval(expr.clone(), assignments_map.clone(), false)?;
                        }
                    }
                }
                Ok(make_thunk!(
                    Rc::new(Expression::Value(return_expr.clone())),
                    assignments_map,
                    false
                ))
            }
        }
    }

    // Makes one lazy step in evaluation of the expression
    // returns new expression
    // similar to beta reduction with normal reduction strategy
    fn eval_expression(
        &mut self,
        expr: Rc<Expression>,
        assignments: HashTrieMap<u32, Rc<Expression>>,
        pure: bool,
    ) -> Result<Rc<Expression>, EvaluatorError> {
        //println!("expr: {:?}", expr);
        let result = match expr.as_ref() {
            Expression::Value(_) => expr.clone(),
            Expression::Thunk(refcell, env, pure) => {
                // if let Some(expression) =
                //     self.thunk_memoization_map.get(&ByThinAddress(expr.clone()))
                // {
                //     return Ok(expression.clone());
                // }
                let inside_rc = refcell.replace(Rc::new(Expression::Value(Value::Nil)));
                match inside_rc.as_ref() {
                    Expression::Value(..) => {
                        // if it is a value, then put it back inside and return it
                        refcell.replace(inside_rc.clone());
                        inside_rc
                    }
                    _ => {
                        // if it is not a value, then evaluate it and put the result inside
                        let result = self.eval_expression(inside_rc.clone(), env.clone(), *pure)?;
                        refcell.replace(result.clone());
                        
                        // // super important for the full memoization
                        // self.thunk_memoization_map
                        //     .insert(ByThinAddress(expr), result.clone());
                        result
                    }
                }
            }
            Expression::Ident(ident) => {
                if let Some(expr) = assignments.get(ident) {
                    make_thunk!(expr.clone(), assignments, pure)
                } else {
                    panic!("Ident should always be in the assignments map")
                    //return Err(EvaluatorError::UnknownName(expr));
                }
            }
            Expression::FunctionCall { expr, args } => {
                match self.force_eval(expr.clone(), assignments.clone(), pure)? {
                    Value::Function { params, scope } => {
                        if pure && matches!(scope.as_ref(), Scope::Impure { .. }) {
                            return Err(EvaluatorError::SideEffectInPureScope(expr.clone()));
                        }
                        if args.len() != params.len() {
                            return Err(EvaluatorError::ArgsAndParamsLengthsMismatch(expr.clone()));
                        }

                        // group params and args and add to environment
                        let assignments = params.into_iter().zip(args.into_iter()).fold(
                            assignments.clone(),
                            |acc, (ident, expr)| {
                                acc.insert(
                                    ident,
                                    make_thunk!(
                                        expr,
                                        assignments,
                                        matches!(scope.as_ref(), Scope::Pure { .. })
                                    ),
                                )
                            },
                        );
                        self.eval_scope(&*scope, assignments)?
                    }
                    _ => {
                        return Err(EvaluatorError::FunctionExpected(expr.clone()));
                    }
                }
            }
            Expression::ReadCall => {
                // Reads must have been evaluated in the evaluation of impure scope
                return Err(EvaluatorError::UnexpectedRead());
            }
            Expression::PrintCall {
                expr: inside_expr,
                newline,
            } => {
                if pure {
                    return Err(EvaluatorError::SideEffectInPureScope(expr.clone()));
                }
                let value = self.force_eval(inside_expr.clone(), assignments, pure)?;
                print(&value, &mut self.output);
                if *newline {
                    writeln!(self.output).unwrap();
                }
                self.output.flush().unwrap();

                Rc::new(Expression::Value(Value::Nil))
            }
            Expression::Cons(left, right) => match (left.as_ref(), right.as_ref()) {
                (Expression::Value(..), Expression::Value(..))
                | (Expression::Value(..), Expression::Thunk(..))
                | (Expression::Thunk(..), Expression::Value(..))
                | (Expression::Thunk(..), Expression::Thunk(..)) => {
                    Rc::new(Expression::Value(Value::Tuple(
                        Box::new(self.force_eval(left.clone(), assignments.clone(), pure)?),
                        Box::new(self.force_eval(right.clone(), assignments, pure)?),
                    )))
                }
                (Expression::Value(_), _) => Rc::new(Expression::Cons(
                    left.clone(),
                    make_thunk!(right.clone(), assignments, pure),
                )),
                (_, Expression::Value(_)) => Rc::new(Expression::Cons(
                    make_thunk!(left.clone(), assignments, pure),
                    right.clone(),
                )),
                (_, _) => Rc::new(Expression::Cons(
                    make_thunk!(left.clone(), assignments.clone(), pure),
                    make_thunk!(right.clone(), assignments, pure),
                )),
            },
            Expression::Left(inside_expr) => match inside_expr.as_ref() {
                Expression::Cons(left, _) => left.clone(),
                Expression::Value(Value::Tuple(left, _)) => {
                    Rc::new(Expression::Value(*left.clone()))
                }
                Expression::Value(_) => {
                    return Err(EvaluatorError::InvalidUnaryOperation {
                        msg: String::from("Left is defined only for cons and tuple"),
                        expr: expr.clone(),
                    })
                }
                _ => Rc::new(Expression::Left(self.eval_expression(
                    inside_expr.clone(),
                    assignments,
                    pure,
                )?)),
            },
            Expression::Right(inside_expr) => match inside_expr.as_ref() {
                Expression::Cons(_, right) => right.clone(),
                Expression::Value(Value::Tuple(_, right)) => {
                    Rc::new(Expression::Value(*right.clone()))
                }
                Expression::Value(_) => {
                    return Err(EvaluatorError::InvalidUnaryOperation {
                        msg: String::from("Right is defined only for cons and tuple"),
                        expr: expr.clone(),
                    })
                }
                _ => Rc::new(Expression::Right(self.eval_expression(
                    inside_expr.clone(),
                    assignments,
                    pure,
                )?)),
            },
            Expression::Empty(inside_expr) => match inside_expr.as_ref() {
                Expression::Value(Value::Nil) => Rc::new(Expression::Value(Value::Boolean(true))),
                Expression::Cons(_, _) => Rc::new(Expression::Value(Value::Boolean(false))),
                Expression::Value(Value::Tuple(_, _)) => {
                    Rc::new(Expression::Value(Value::Boolean(false)))
                }
                Expression::Value(_) => {
                    return Err(EvaluatorError::InvalidUnaryOperation {
                        msg: String::from("Empty is defined only for cons and tuple"),
                        expr: expr.clone(),
                    })
                }
                _ => Rc::new(Expression::Empty(self.eval_expression(
                    inside_expr.clone(),
                    assignments,
                    pure,
                )?)),
            },
            Expression::UnaryOperation(op, inside_expr) => {
                Rc::new(Expression::Value(eval_unary_op(
                    *op,
                    &self.force_eval(inside_expr.clone(), assignments.clone(), pure)?,
                )?))
            }
            Expression::BinaryOperation(op, left, right) => {
                Rc::new(Expression::Value(eval_bin_op(
                    *op,
                    &self.force_eval(left.clone(), assignments.clone(), pure)?,
                    &self.force_eval(right.clone(), assignments.clone(), pure)?,
                )?))
            }
            Expression::If {
                condition,
                then_scope,
                else_scope,
            } => match self.force_eval(condition.clone(), assignments.clone(), pure)? {
                Value::Boolean(true) => self.eval_scope(&**then_scope, assignments)?,
                Value::Boolean(false) => self.eval_scope(&**else_scope, assignments)?,
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
