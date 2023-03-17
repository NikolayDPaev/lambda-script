#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Token {
    Name(String),
    Str(String),
    Char(String),
    Number(String),
    Operation(Op),
    True,
    False,
    If,
    Else,
    Then,
    Assignment,
    Arrow,
    LeftBracket,
    RightBracket,
    Comma,
    LeftBoxBracket,
    RightBoxBracket,
    Impure,
    Cons,
    Left,
    Right,
    Empty,
    Nil,
    Import,
    Once,
    Read,
    Print,
    Println
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Op {
    And,
    Or,
    Xor,
    Negation,
    Plus,
    Minus,
    Division,
    IntDivision,
    Multiplication,
    Exponentiation,
    Modulo,
    NEq,
    Eq,
    Lt,
    Gt,
    LEq,
    GEq,
}

impl Op {
    pub fn precedence(&self) -> u8 {
        match self {
            Op::Negation => 0,
            Op::Or => 1,
            Op::Xor => 2,
            Op::And => 3,
            Op::NEq => 4,
            Op::Eq => 4,
            Op::Lt =>  5,
            Op::Gt =>  5,
            Op::LEq => 5,
            Op::GEq => 5,
            Op::Plus => 6,
            Op::Minus => 6,
            Op::Division => 7,
            Op::IntDivision => 7,
            Op::Multiplication => 7,
            Op::Exponentiation => 7,
            Op::Modulo => 7,
        }
    }
}
