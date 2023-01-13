use std::{rc::Rc, cmp::Ordering, fmt::{Formatter, self}};

use rpds::HashTrieMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Scope {
    Pure{
        assignments: Vec<(String, Rc<Expression>)>,
        expression: Rc<Expression>,
    },
    NonPure{
        assignments: Vec<(String, Rc<Expression>)>,
        statements: Vec<Rc<Expression>>,
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Number {
    Float(f64),
    Integer(i32),
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for Number {}

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
            (Number::Float(a), Number::Integer(b)) => float_cmp(a, &(*b as f64)),
            (Number::Integer(a), Number::Float(b)) => float_cmp(&(*a as f64), b),
            (Number::Integer(a), Number::Integer(b)) => a.cmp(b),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Boolean(bool),
    Nil,
    Number(Number),
    Char(char),
    Error(String),
    Tuple(Box<Value>, Box<Value>),
    Function {
        params: Vec<String>,
        scope: Box<Scope>,
    }
}

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

fn fmt(tree: &HashTrieMap<String, Rc<Expression>>, f: &mut Formatter) -> fmt::Result {
    write!(f, "{:?}", tree.iter().filter(|(_, expr)| {
        match expr.as_ref() {
            Expression::Value(Value::Function { .. }) => false,
            Expression::Value(_) => true,
            _ => false,
        }
    }).collect::<Vec<_>>())
}

#[derive(Educe)]
#[educe(Debug)]
#[derive(PartialEq, Clone)]
pub enum Expression {
    Value(Value),
    Name(String),
    Thunk(
        Rc<Expression>,
        #[educe(Debug(method = "fmt"))]
        HashTrieMap<String, Rc<Expression>>,
        bool,
    ),
    FunctionCall {
        name: Rc<Expression>,
        args: Vec<Rc<Expression>>,
    },
    ReadCall,
    PrintCall(Rc<Expression>),
    Cons(Rc<Expression>,Rc<Expression>),
    Left(Rc<Expression>),
    Right(Rc<Expression>),
    UnaryOperation(UnaryOp, Rc<Expression>),
    BinaryOperation(BinaryOp, Rc<Expression>, Rc<Expression>),
    If {
        condition: Rc<Expression>,
        then_scope: Box<Scope>,
        else_scope: Box<Scope>,
    }
}
