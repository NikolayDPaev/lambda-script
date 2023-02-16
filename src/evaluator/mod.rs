pub mod errors;
mod operations;
#[cfg(test)]
mod tests;

use by_address::ByThinAddress;
use rpds::HashTrieMap;
use std::io::{BufRead, BufReader, BufWriter, Read, Write};
use std::{collections::HashMap, rc::Rc};

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
                $expr.clone(),
                $assignments.clone(),
                $pure,
            )),
            // if the expression is thunk or value, it does not need its context
            Expression::Thunk(..) | Expression::Value(..) => $expr.clone(),
            _ => Rc::new(Expression::Thunk(
                $expr.clone(),
                $assignments.clone(),
                $pure,
            )),
        }
    };
}

fn try_resolve_name(
    expr: Rc<Expression>,
    env: HashTrieMap<String, Rc<Expression>>,
) -> Result<Rc<Expression>, EvaluatorError> {
    let mut expression = expr;
    loop {
        match expression.as_ref() {
            Expression::Name(name) => {
                if let Some(expr) = env.get(name) {
                    expression = expr.clone();
                } else {
                    return Err(EvaluatorError::UnknownName(expression));
                }
            }
            _ => return Ok(expression),
        }
    }
}

pub struct Evaluator<'a, R: Read, W: Write> {
    thunk_memoization_map: HashMap<ByThinAddress<Rc<Expression>>, Rc<Expression>>,
    input: &'a mut BufReader<R>,
    output: &'a mut BufWriter<W>,
    debug: bool,
}

impl<R, W> Evaluator<'_, R, W>
where
    R: Read,
    W: Write,
{
    pub fn new<'a>(
        input: &'a mut BufReader<R>,
        output: &'a mut BufWriter<W>,
        debug: bool,
    ) -> Evaluator<'a, R, W> {
        Evaluator {
            thunk_memoization_map: HashMap::new(),
            input,
            output,
            debug,
        }
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

    // Evaluates the expression to a value and if it is a thunk, memoizes the result
    fn force_eval(
        &mut self,
        expr: Rc<Expression>,
        assignments: HashTrieMap<String, Rc<Expression>>,
        pure: bool,
    ) -> Result<Value, EvaluatorError> {
        let mut expression = expr.clone();
        let mut thunks_for_memo = vec![];
        loop {
            if pure && matches!(expression.as_ref(), Expression::Thunk(..)) {
                thunks_for_memo.push(expression.clone());
            }
            if let Expression::Value(value) = expression.as_ref() {
                while let Some(thunk) = thunks_for_memo.pop() {
                    self.thunk_memoization_map
                        .insert(ByThinAddress(thunk.clone()), expression.clone());
                }
                return Ok(value.clone());
            }
            expression = self.eval_expression(expression, assignments.clone(), pure)?
        }
    }

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

    fn eval_scope(
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
            Scope::Impure { lines } => {
                // add the outside assignments
                // force eval the assignments and the expressions
                // eval the read calls
                let mut assignments_map = outside_assignments.clone();
                let mut return_expr = Value::Nil;
                for line in lines {
                    match line {
                        ImpureLine::Assignment(name, expr) => {
                            let evaluated_expr = match expr.as_ref() {
                                // if assignment expression is read call, force its evaluation
                                Expression::ReadCall => self.force_read(),
                                _ => Rc::new(Expression::Value(self.force_eval(
                                    expr.clone(),
                                    assignments_map.clone(),
                                    false,
                                )?)),
                            };
                            assignments_map =
                                assignments_map.insert(name.to_string(), evaluated_expr);
                            return_expr = Value::Nil;
                        }
                        ImpureLine::Expression(expr) => {
                            return_expr = self.force_eval(
                                expr.clone(),
                                assignments_map.clone(),
                                false,
                            )?;
                        }
                    }
                }
                Ok(make_thunk!(Rc::new(Expression::Value(return_expr.clone())), assignments_map, false))
            }
        }
    }

    fn eval_expression(
        &mut self,
        expr: Rc<Expression>,
        assignments: HashTrieMap<String, Rc<Expression>>,
        pure: bool,
    ) -> Result<Rc<Expression>, EvaluatorError> {
        if self.debug {
            writeln!(self.output, "Evaluating: {:?}", expr).unwrap();
        }
        let result = match expr.as_ref() {
            Expression::Value(_) => expr.clone(),
            Expression::Thunk(inside_expr, env, pure) => {
                if let Some(expression) =
                    self.thunk_memoization_map.get(&ByThinAddress(expr.clone()))
                {
                    if self.debug {
                        writeln!(
                            self.output,
                            "Using memoization for: {:?} -> {:?}",
                            expr, expression
                        )
                        .unwrap();
                    }
                    return Ok(expression.clone());
                }
                let result = self.eval_expression(inside_expr.clone(), env.clone(), *pure)?;
                
                // super important for the full memoization
                self.thunk_memoization_map.insert(ByThinAddress(expr), result.clone());
                result
            }
            Expression::Name(name) => {
                if let Some(expr) = assignments.get(name) {
                    make_thunk!(expr.clone(), assignments, pure)
                } else {
                    return Err(EvaluatorError::UnknownName(expr));
                }
            }
            Expression::FunctionCall { name, args } => {
                match self.force_eval(name.clone(), assignments.clone(), pure)? {
                    Value::Function { params, scope } => {
                        if pure && matches!(scope.as_ref(), Scope::Impure { .. }) {
                            return Err(EvaluatorError::SideEffectInPureScope(name.clone()));
                        }
                        if args.len() != params.len() {
                            return Err(EvaluatorError::ArgsAndParamsLengthsMismatch(name.clone()));
                        }
                        // resolve names of args
                        let mut resolved_args = vec![];
                        for arg in args.iter() {
                            resolved_args.push(try_resolve_name(arg.clone(), assignments.clone())?);
                        }

                        // group params and args and add to environment
                        let assignments = params.into_iter().zip(resolved_args.into_iter()).fold(
                            assignments.clone(),
                            |acc, (string, expr)| {
                                acc.insert(
                                    string.clone(),
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
                        return Err(EvaluatorError::FunctionExpected(name.clone()));
                    }
                }
            }
            Expression::ReadCall => {
                // Reads must have been evaluated in the evaluation of impure scope
                return Err(EvaluatorError::UnexpectedRead());
            }
            Expression::PrintCall(inside_expr) => {
                if pure {
                    return Err(EvaluatorError::SideEffectInPureScope(expr.clone()));
                }
                let value = self.force_eval(inside_expr.clone(), assignments, pure)?;
                print(&value, &mut self.output);
                writeln!(self.output).unwrap();
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
