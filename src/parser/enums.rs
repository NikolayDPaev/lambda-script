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
    Cons,
    Boolean(BoolBinOp),
    Number(NumberBinOp),
    Compare(CmpBinOp),
}

pub enum UnaryOp {
    Negation,
    Minus,
    Left,
    Right,
}

pub enum Expression {
    Value(Value),
    FunctionCall {
        args: Vec<Box<Expression>>,
    },
    ReadCall,
    PrintCall(Box<Expression>),
    UnaryOperation(UnaryOp, Box<Expression>),
    BinaryOperation(BinaryOp, Box<Expression>, Box<Expression>),
    If {
        condition: Box<Expression>,
        then_expression: Box<Expression>,
        else_expression: Box<Expression>,
    }
}
