pub struct Scope {
    pub assignments: Vec<(String, Expression)>,
    pub expression: Expression,
}

pub enum Number {
    Float(f64),
    Integer(i32),
}

pub enum Value {
    True,
    False,
    Tuple(Box<Value>, Box<Value>),
    Nil,
    Number(Number),
    Char(char),
    Error(String),
    Function {
        pure: bool,
        args: Vec<String>,
        assignments: Vec<(String, Expression)>,
        returns: Box<Expression>,
    }
}

pub enum BoolBinOp {
    And,
    Or,
    Xor,
}

pub enum NumberBinOp {
    Plus,
    Minus,
    Division,
    Multiplication,
    Exponentiation,
    Modulo,
}

pub enum CmpBinOp {
    Eq,
    Lt,
    Gt,
    LEq,
    GEq,
}

pub enum BinaryOp {
    Boolean(BoolBinOp),
    Number(NumberBinOp),
    Compare(CmpBinOp),
}

pub enum UnaryOp {
    Negation,
    Minus,
}

pub enum Expression {
    Value(Value),
    Name(String),
    FunctionCall {
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
