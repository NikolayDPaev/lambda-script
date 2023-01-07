use std::rc::Rc;

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
pub enum NumberBinOp {
    Plus,
    Minus,
    Division,
    Multiplication,
    Exponentiation,
    Modulo,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CmpBinOp {
    Eq,
    Lt,
    Gt,
    LEq,
    GEq,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BinaryOp {
    Boolean(BoolBinOp),
    Number(NumberBinOp),
    Compare(CmpBinOp),
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum UnaryOp {
    Negation,
    Minus,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    Value(Value),
    Name(String),
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
