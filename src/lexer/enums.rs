use std::fmt;

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
    Println,
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
            Op::Lt => 5,
            Op::Gt => 5,
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
impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let string = match self {
            Op::And => " & ",
            Op::Or => " | ",
            Op::Xor => " ^ ",
            Op::Negation => " !",
            Op::Plus => " + ",
            Op::Minus => " - ",
            Op::Division => " / ",
            Op::IntDivision => " // ",
            Op::Multiplication => " * ",
            Op::Exponentiation => " ** ",
            Op::Modulo => " % ",
            Op::NEq => " != ",
            Op::Eq => " == ",
            Op::Lt => " < ",
            Op::Gt => " > ",
            Op::LEq => " <= ",
            Op::GEq => " >= ",
        };
        write!(f, "{}", string)
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Token::Name(name) => write!(f, "{}", name),
            Token::Str(string) => write!(f, "{:?}", string),
            Token::Char(c) => write!(f, "'{}'", c),
            Token::Number(num) => write!(f, "{}", num),
            Token::Operation(op) => write!(f, "{}", op),
            Token::True => write!(f, "true"),
            Token::False => write!(f, "false"),
            Token::If => write!(f, "if "),
            Token::Else => write!(f, " else "),
            Token::Then => write!(f, " then "),
            Token::Assignment => write!(f, " = "),
            Token::Arrow => write!(f, " -> "),
            Token::LeftBracket => write!(f, "("),
            Token::RightBracket => write!(f, ")"),
            Token::Comma => write!(f, ", "),
            Token::LeftBoxBracket => write!(f, "["),
            Token::RightBoxBracket => write!(f, "]"),
            Token::Impure => write!(f, "impure "),
            Token::Cons => write!(f, "cons"),
            Token::Left => write!(f, "left"),
            Token::Right => write!(f, "right"),
            Token::Empty => write!(f, "empty"),
            Token::Nil => write!(f, "nil"),
            Token::Import => write!(f, "import "),
            Token::Once => write!(f, "once "),
            Token::Read => write!(f, "read"),
            Token::Print => write!(f, "print"),
            Token::Println => write!(f, "println"),
        }
    }
}
