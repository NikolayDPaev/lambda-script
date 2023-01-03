#[derive(Debug, PartialEq)]
pub struct Scope {
    pub assignments: Vec<(String, Expression)>,
    pub expression: Expression,
}

#[derive(Debug, PartialEq)]
pub enum Number {
    Float(f64),
    Integer(i32),
}

#[derive(Debug, PartialEq)]
pub enum Value {
    Boolean(bool),
    Nil,
    Number(Number),
    Char(char),
    Error(String),
    Function {
        pure: bool,
        params: Vec<String>,
        scope: Box<Scope>,
    }
}

#[derive(Debug, PartialEq)]
pub enum BoolBinOp {
    And,
    Or,
    Xor,
}

#[derive(Debug, PartialEq)]
pub enum NumberBinOp {
    Plus,
    Minus,
    Division,
    Multiplication,
    Exponentiation,
    Modulo,
}

#[derive(Debug, PartialEq)]
pub enum CmpBinOp {
    Eq,
    Lt,
    Gt,
    LEq,
    GEq,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Boolean(BoolBinOp),
    Number(NumberBinOp),
    Compare(CmpBinOp),
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Negation,
    Minus,
}

#[derive(Debug, PartialEq)]
pub enum Expression {
    Value(Value),
    Name(String),
    FunctionCall {
        name: String,
        args: Vec<Expression>,
    },
    ReadCall,
    PrintCall(Box<Expression>),
    Cons(Box<Expression>,Box<Expression>),
    Left(Box<Expression>),
    Right(Box<Expression>),
    UnaryOperation(UnaryOp, Box<Expression>),
    BinaryOperation(BinaryOp, Box<Expression>, Box<Expression>),
    If {
        condition: Box<Expression>,
        then_scope: Box<Scope>,
        else_scope: Box<Scope>,
    }
}
