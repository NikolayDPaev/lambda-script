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
