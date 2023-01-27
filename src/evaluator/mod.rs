mod operations;
#[cfg(test)]
mod tests;
use by_address::ByAddress;
use rpds::HashTrieMap;
use std::io::{Write, Read, BufRead, BufReader, BufWriter};
use std::{collections::HashMap, rc::Rc};

use crate::evaluator::operations::*;
use crate::parser::enums::*;

#[derive(Debug)]
pub enum EvaluatorError {
    UnknownName(Rc<Expression>),
    FunctionExpected(Rc<Expression>),
    ArgsAndParamsLengthsMismatch(Rc<Expression>),
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

pub fn process_evaluator_error(err: EvaluatorError) -> String {
    match err {
        EvaluatorError::UnknownName(expression) => format!("Use of undeclared name in this scope: {:?}", expression),
        EvaluatorError::FunctionExpected(expression) => format!("Expected function, actual expression is: {:?}", expression),
        EvaluatorError::ArgsAndParamsLengthsMismatch(expression) => format!("Wrong number of arguments provided to the functions: {:?}", expression),
        EvaluatorError::InvalidOperation { msg, expr } => format!("Invalid operation in the expression: {:?} {:?}", msg, expr),
        EvaluatorError::SideEffectInPureScope(expr) => format!("Expression produces side effect in pure scope: {:?}", expr),
        EvaluatorError::ConditionShouldEvaluateToBoolean(expr) => format!("Expression was expected to evaluate to boolean: {:?}", expr),
        EvaluatorError::ComparisonError { op, value_1, value_2 } => format!("{:?} and {:?} cannot be compared with {:?}", value_1, value_2, op),
        EvaluatorError::ArithmeticError { op, value_1, value_2 } => format!("Operation {:?} cannot be applied to {:?} and {:?}", op, value_1, value_2),
    }
}

pub struct Evaluator<'a, R : Read, W: Write> {
    memoization_map: HashMap<ByAddress<Rc<Expression>>, Value>,
    input: &'a mut BufReader<R>,
    output: &'a mut BufWriter<W>,
    debug: bool
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

impl<R, W> Evaluator<'_, R, W> where R: Read, W: Write {
    pub fn new<'a>(input: &'a mut BufReader<R>, output: &'a mut BufWriter<W>, debug: bool) -> Evaluator<'a, R, W> {
        Evaluator {
            memoization_map: HashMap::new(),
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

    fn force_eval(
        &mut self,
        expr: Rc<Expression>,
        assignments: HashTrieMap<String, Rc<Expression>>,
        memoize: bool,
    ) -> Result<Value, EvaluatorError> {
        let mut expression = expr.clone();
        loop {
            if let Expression::Value(value) = expression.as_ref() {
                return Ok(value.clone());
            }
            //println!("Evaluating expression: {:?}", expression);
            expression = self.eval_expression(expression, assignments.clone(), memoize)?
        }
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
            Scope::NonPure {
                assignments,
                statements,
            } => {
                let assignments_map = add_outside_assignments!(outside_assignments, assignments);
                // let mut assignments_map = outside_assignments;
                // for (name, expr) in assignments.iter() {
                //     assignments_map = assignments_map.insert(name.clone(), Rc::new(Expression::Value(self.force_eval(expr.clone(), assignments_map.clone(), false)?)));
                // }
                let mut last_expr = Rc::new(Expression::Value(Value::Nil));
                for expr in statements {
                    last_expr = Rc::new(Expression::Value(self.force_eval(
                        expr.clone(),
                        assignments_map.clone(),
                        false,
                    )?));
                }
                Ok(make_thunk!(last_expr.clone(), assignments_map, false))
            }
        }
    }

    fn eval_expression(
        &mut self,
        expr: Rc<Expression>,
        assignments: HashTrieMap<String, Rc<Expression>>,
        memoize: bool,
    ) -> Result<Rc<Expression>, EvaluatorError> {
        if self.debug {
            self.output.write_fmt(format_args!("Evaluating: {:?}\n", expr)).unwrap();
        }
        // if let Some(expression) = self.memoization_map.get(&ByAddress(expr.clone())) {
        //     //println!("using memoization for: {:?}", expression);
        //     return Ok(Rc::new(Expression::Value(expression.clone())));
        // } else {
            let expr = try_resolve_name(expr, assignments.clone())?;
            let result = match expr.as_ref() {
                Expression::Value(_) => expr.clone(),
                Expression::Thunk(expr, env, memoize) => {
                    if matches!(expr.as_ref(), Expression::ReadCall) {
                        let value = self.force_eval(expr.clone(), env.clone(), *memoize)?;
                        self.memoization_map
                            .insert(ByAddress(expr.clone()), value.clone());
                        Rc::new(Expression::Value(value))
                    } else {
                        self.eval_expression(expr.clone(), env.clone(), *memoize)?
                    }
                }
                Expression::Name(_) => panic!("expression is name after resolving"),
                Expression::FunctionCall { name, args } => {
                    match self.force_eval(name.clone(), assignments.clone(), memoize)? {
                        Value::Function { params, scope } => {
                            if memoize && matches!(scope.as_ref(), Scope::NonPure { .. }) {
                                return Err(EvaluatorError::SideEffectInPureScope(name.clone()));
                            }
                            if args.len() != params.len() {
                                return Err(EvaluatorError::ArgsAndParamsLengthsMismatch(
                                    name.clone(),
                                ));
                            }
                            // resolve names of args
                            let mut resolved_args = vec![];
                            for arg in args.iter() {
                                resolved_args
                                    .push(try_resolve_name(arg.clone(), assignments.clone())?);
                            }
                            // group params and args and add to environment
                            let assignments = params
                                .into_iter()
                                .zip(resolved_args.into_iter())
                                .fold(assignments.clone(), |acc, (string, expr)| {
                                    acc.insert(
                                        string.clone(),
                                        make_thunk!(
                                            expr,
                                            assignments,
                                            matches!(scope.as_ref(), Scope::NonPure { .. })
                                        ),
                                    )
                                });
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
                    if let Some(expression) = self.memoization_map.get(&ByAddress(expr.clone())) {
                        return Ok(Rc::new(Expression::Value(expression.clone())));
                    }
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
                Expression::PrintCall(inside_expr) => {
                    if memoize {
                        return Err(EvaluatorError::SideEffectInPureScope(expr.clone()));
                    }
                    let value = self.force_eval(inside_expr.clone(), assignments, memoize)?;
                    print(&value, &mut self.output);
                    self.output.write(b"\n").unwrap();
                    self.output.flush().unwrap();
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
                    Expression::Value(Value::Nil) => {
                        Rc::new(Expression::Value(Value::Boolean(true)))
                    }
                    Expression::Cons(_, _) => Rc::new(Expression::Value(Value::Boolean(false))),
                    Expression::Value(Value::Tuple(_, _)) => {
                        Rc::new(Expression::Value(Value::Boolean(false)))
                    }
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
                    Rc::new(Expression::Value(eval_unary_op(
                        *op,
                        &self.force_eval(inside_expr.clone(), assignments.clone(), memoize)?,
                    )?))
                }
                Expression::BinaryOperation(op, left, right) => {
                    Rc::new(Expression::Value(eval_bin_op(
                        *op,
                        &self.force_eval(left.clone(), assignments.clone(), memoize)?,
                        &self.force_eval(right.clone(), assignments.clone(), memoize)?,
                    )?))
                }
                Expression::If {
                    condition,
                    then_scope,
                    else_scope,
                } => match self.force_eval(condition.clone(), assignments.clone(), memoize)? {
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
//}
