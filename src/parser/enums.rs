use std::{
    cmp::Ordering,
    fmt::{self, Formatter},
    rc::Rc,
};

use rpds::HashTrieMap;

use super::FunctionLine;
pub type ImpureLine = FunctionLine;

#[derive(Debug, PartialEq, Clone)]
pub enum Scope {
    Pure {
        assignments: Vec<(u32, Rc<Expression>)>,
        expression: Rc<Expression>,
    },
    Impure {
        lines: Vec<ImpureLine>,
    },
}

#[derive(Educe)]
#[educe(Debug)]
#[derive(PartialEq, Clone)]
pub enum Expression {
    Value(Value),
    Ident(u32),
    Thunk(
        Rc<Expression>,
        #[educe(Debug(method = "fmt"))] HashTrieMap<u32, Rc<Expression>>,
        #[educe(Debug(ignore))] bool,
    ),
    FunctionCall {
        expr: Rc<Expression>,
        args: Vec<Rc<Expression>>,
    },
    ReadCall,
    PrintCall {
        expr: Rc<Expression>,
        newline: bool,
    },
    Cons(Rc<Expression>, Rc<Expression>),
    Left(Rc<Expression>),
    Right(Rc<Expression>),
    Empty(Rc<Expression>),
    UnaryOperation(UnaryOp, Rc<Expression>),
    BinaryOperation(BinaryOp, Rc<Expression>, Rc<Expression>),
    If {
        condition: Rc<Expression>,
        then_scope: Box<Scope>,
        else_scope: Box<Scope>,
    },
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Boolean(bool),
    Nil,
    Number(Number),
    Char(char),
    Tuple(Box<Value>, Box<Value>),
    Function { params: Vec<u32>, scope: Box<Scope> },
}

// for debugging of hashTrieMap
fn fmt(tree: &HashTrieMap<u32, Rc<Expression>>, f: &mut Formatter) -> fmt::Result {
    write!(
        f,
        "{:?}",
        tree.iter()
            .filter(|(_, expr)| {
                match expr.as_ref() {
                    Expression::Value(Value::Function { .. }) => false,
                    Expression::Value(_) => true,
                    _ => false,
                }
            })
            .collect::<Vec<_>>()
    )
}

#[derive(Debug, Clone)]
pub enum Number {
    Float(f64),
    Integer(i32),
}

fn float_cmp(val_1: &f64, val_2: &f64) -> Ordering {
    if (val_1 - val_2).abs() < f64::EPSILON {
        Ordering::Equal
    } else if val_1 - val_2 > f64::EPSILON {
        Ordering::Greater
    } else {
        Ordering::Less
    }
}

impl Ord for Number {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Number::Float(a), Number::Float(b)) => float_cmp(a, b),
            (Number::Float(a), Number::Integer(b)) => float_cmp(a, &(Into::<f64>::into(*b))),
            (Number::Integer(a), Number::Float(b)) => float_cmp(&(Into::<f64>::into(*a)), b),
            (Number::Integer(a), Number::Integer(b)) => a.cmp(b),
        }
    }
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for Number {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Number {}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BoolBinOp {
    And,
    Or,
    Xor,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum ArithBinOp {
    Plus,
    Minus,
    Division,
    IntDivision,
    Multiplication,
    Exponentiation,
    Modulo,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CmpBinOp {
    Eq,
    NEq,
    Lt,
    Gt,
    LEq,
    GEq,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOp {
    Boolean(BoolBinOp),
    Arithmetic(ArithBinOp),
    Compare(CmpBinOp),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Negation,
    Minus,
}

pub fn display_value(value: &Value, names: &[String]) -> String {
    match value {
        Value::Boolean(boolean) => format!("{}", boolean),
        Value::Nil => format!("nil"),
        Value::Number(Number::Integer(i)) => format!("{}", i),
        Value::Number(Number::Float(f)) => format!("{}", f),
        Value::Char(c) => format!("'{}'", c),
        Value::Tuple(v1, v2) => format!(
            "({}, {})",
            display_value(v1, names),
            display_value(v2, names)
        ),
        Value::Function { params, scope: _ } => format!(
            "function {:?}",
            params
                .into_iter()
                .map(|ident| names[*ident as usize].to_owned())
                .collect::<Vec<String>>()
        ),
    }
}

pub fn display_expr(expr: Rc<Expression>, names: &[String]) -> String {
    match expr.as_ref() {
        Expression::Value(value) => format!("Value({})", display_value(value, names)),
        Expression::Ident(ident) => format!("{}", names[*ident as usize]),
        Expression::Thunk(expr, env, _) => format!(
            "Thunk({}, env: {:?})",
            display_expr(expr.clone(), names),
            env.iter()
                .filter(|(_, expr)| {
                    match expr.as_ref() {
                        Expression::Value(Value::Function { .. }) => false,
                        Expression::Value(_) => true,
                        _ => false,
                    }
                })
                .collect::<Vec<_>>()
        ),
        Expression::FunctionCall { expr, args } => format!(
            "FunctionCall({}, {:?})",
            display_expr(expr.clone(), names),
            args.into_iter()
                .map(|expr| display_expr(expr.clone(), names))
                .collect::<Vec<String>>()
        ),
        Expression::ReadCall => format!("Read"),
        Expression::PrintCall { expr, newline: _ } => format!("Print({})", display_expr(expr.clone(), names)),
        Expression::Cons(expr1, expr2) => format!("Cons({}, {})", display_expr(expr1.clone(), names), display_expr(expr2.clone(), names)),
        Expression::Left(expr) => format!("Left({})", display_expr(expr.clone(), names)),
        Expression::Right(expr) => format!("Right({})", display_expr(expr.clone(), names)),
        Expression::Empty(expr) => format!("Empty({})", display_expr(expr.clone(), names)),
        Expression::UnaryOperation(op, expr) => format!("UnaryOperation({:?}, {})", op, display_expr(expr.clone(), names)),
        Expression::BinaryOperation(op, expr1, expr2) => format!("BinaryOperation({:?}, {}, {})", op, display_expr(expr1.clone(), names), display_expr(expr2.clone(), names)),
        Expression::If {
            condition,
            then_scope: _,
            else_scope: _,
        } => format!("If({})", display_expr(condition.clone(), names))
    }
}
