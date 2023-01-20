use std::{rc::Rc, cmp::Ordering, fmt::{Formatter, self}, cell::RefCell};

use rpds::HashTrieMap;

#[derive(Debug, PartialEq, Clone)]
pub enum Scope {
    Pure{
        assignments: Vec<(String, Rc<RefCell<Expression>>)>,
        expression: Rc<RefCell<Expression>>,
    },
    NonPure{
        assignments: Vec<(String, Rc<RefCell<Expression>>)>,
        statements: Vec<Rc<RefCell<Expression>>>,
    }
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

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Boolean(bool),
    Nil,
    Number(Number),
    Char(char),
    Tuple(Box<Value>, Box<Value>),
    Function {
        params: Vec<String>,
        scope: Box<RefCell<Scope>>,
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

fn fmt(tree: &HashTrieMap<String, Rc<RefCell<Expression>>>, f: &mut Formatter) -> fmt::Result {
    write!(f, "{:?}", tree.iter().filter(|(_, expr)| {
        match *expr.borrow() {
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
        Rc<RefCell<Expression>>,
        #[educe(Debug(method = "fmt"))]
        HashTrieMap<String, Rc<RefCell<Expression>>>,
        #[educe(Debug(ignore))]
        bool,
    ),
    FunctionCall {
        name: Rc<RefCell<Expression>>,
        args: Vec<Rc<RefCell<Expression>>>,
    },
    ReadCall,
    PrintCall(Rc<RefCell<Expression>>),
    Cons(Rc<RefCell<Expression>>,Rc<RefCell<Expression>>),
    Left(Rc<RefCell<Expression>>),
    Right(Rc<RefCell<Expression>>),
    Empty(Rc<RefCell<Expression>>),
    UnaryOperation(UnaryOp, Rc<RefCell<Expression>>),
    BinaryOperation(BinaryOp, Rc<RefCell<Expression>>, Rc<RefCell<Expression>>),
    If {
        condition: Rc<RefCell<Expression>>,
        then_scope: Box<RefCell<Scope>>,
        else_scope: Box<RefCell<Scope>>,
    }
}
