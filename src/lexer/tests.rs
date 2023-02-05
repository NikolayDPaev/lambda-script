use super::*;

macro_rules! assert_eq_vec {
    ($vec1:expr, $vec2:expr) => {
        for i in 0..std::cmp::max($vec1.len(), $vec2.len()) {
            assert_eq!($vec1[i], $vec2[i]);
        }
    };
}

#[test]
fn test_all_lexemes_line() {
    let lexemes = "   variable \"\" \"word with space\"   \'\' \'char\' 55.44 1234 0 & | ^ ! + - / // * ** % == != < > <= >= true false if else then = -> ( ) , [ ] impure cons left right empty nil import read print";
    let lines = lines(lexemes.as_bytes()).collect::<Vec<_>>();
    assert_eq!(lines.len(), 1);
    let line = lines[0].as_ref().unwrap();

    let expected = vec![
        Token::Name(String::from("variable")),
        Token::Str(String::from("")),
        Token::Str(String::from("word with space")),
        Token::Char(String::from("")),
        Token::Char(String::from("char")),
        Token::Number(String::from("55.44")),
        Token::Number(String::from("1234")),
        Token::Number(String::from("0")),
        Token::Operation(Op::And),
        Token::Operation(Op::Or),
        Token::Operation(Op::Xor),
        Token::Operation(Op::Negation),
        Token::Operation(Op::Plus),
        Token::Operation(Op::Minus),
        Token::Operation(Op::Division),
        Token::Operation(Op::IntDivision),
        Token::Operation(Op::Multiplication),
        Token::Operation(Op::Exponentiation),
        Token::Operation(Op::Modulo),
        Token::Operation(Op::Eq),
        Token::Operation(Op::NEq),
        Token::Operation(Op::Lt),
        Token::Operation(Op::Gt),
        Token::Operation(Op::LEq),
        Token::Operation(Op::GEq),
        Token::True,
        Token::False,
        Token::If,
        Token::Else,
        Token::Then,
        Token::Assignment,
        Token::Arrow,
        Token::LeftBracket,
        Token::RightBracket,
        Token::Comma,
        Token::LeftBoxBracket,
        Token::RightBoxBracket,
        Token::Impure,
        Token::Cons,
        Token::Left,
        Token::Right,
        Token::Empty,
        Token::Nil,
        Token::Import,
        Token::Read,
        Token::Print,
    ];

    assert_eq_vec!(line.tokens, expected);
}

#[test]
fn test_indentation() {
    let input = "var\n var\n  var\n   var\n\tvar\n\t var\n\t  \tvar\n\t\tvar\n";
    let lines = lines(input.as_bytes()).collect::<Vec<_>>();
    assert_eq!(lines[0].as_ref().unwrap().indentation, 0);
    assert_eq!(lines[0].as_ref().unwrap().number, 1);
    assert_eq!(lines[1].as_ref().unwrap().indentation, 1);
    assert_eq!(lines[1].as_ref().unwrap().number, 2);
    assert_eq!(lines[2].as_ref().unwrap().indentation, 2);
    assert_eq!(lines[2].as_ref().unwrap().number, 3);
    assert_eq!(lines[3].as_ref().unwrap().indentation, 3);
    assert_eq!(lines[3].as_ref().unwrap().number, 4);
    assert_eq!(lines[4].as_ref().unwrap().indentation, 4);
    assert_eq!(lines[4].as_ref().unwrap().number, 5);
    assert_eq!(lines[5].as_ref().unwrap().indentation, 5);
    assert_eq!(lines[5].as_ref().unwrap().number, 6);
    assert_eq!(lines[6].as_ref().unwrap().indentation, 10);
    assert_eq!(lines[6].as_ref().unwrap().number, 7);
    assert_eq!(lines[7].as_ref().unwrap().indentation, 8);
    assert_eq!(lines[7].as_ref().unwrap().number, 8);
}

#[test]
fn test_multiple_lines() {
    let input = "  fun = (a, b) ->\n   sum = a + b\n   sum";
    let lines = lines(input.as_bytes()).collect::<Vec<_>>();
    assert_eq!(lines[0].as_ref().unwrap().indentation, 2);
    assert_eq!(lines[0].as_ref().unwrap().number, 1);
    assert_eq_vec!(
        lines[0].as_ref().unwrap().tokens,
        vec![
            Token::Name(String::from("fun")),
            Token::Assignment,
            Token::LeftBracket,
            Token::Name(String::from("a")),
            Token::Comma,
            Token::Name(String::from("b")),
            Token::RightBracket,
            Token::Arrow,
        ]
    );
    assert_eq!(lines[1].as_ref().unwrap().indentation, 3);
    assert_eq!(lines[1].as_ref().unwrap().number, 2);
    assert_eq_vec!(
        lines[1].as_ref().unwrap().tokens,
        vec![
            Token::Name(String::from("sum")),
            Token::Assignment,
            Token::Name(String::from("a")),
            Token::Operation(Op::Plus),
            Token::Name(String::from("b")),
        ]
    );
    assert_eq!(lines[2].as_ref().unwrap().indentation, 3);
    assert_eq!(lines[2].as_ref().unwrap().number, 3);
    assert_eq_vec!(
        lines[2].as_ref().unwrap().tokens,
        vec![Token::Name(String::from("sum")),]
    );
}

#[test]
fn test_multiple_lines_no_whitespace() {
    let input = "  fun=(a,b)->\n   prod=a*b\n   prod";
    let lines = lines(input.as_bytes()).collect::<Vec<_>>();
    assert_eq!(lines[0].as_ref().unwrap().indentation, 2);
    assert_eq!(lines[0].as_ref().unwrap().number, 1);
    assert_eq_vec!(
        lines[0].as_ref().unwrap().tokens,
        vec![
            Token::Name(String::from("fun")),
            Token::Assignment,
            Token::LeftBracket,
            Token::Name(String::from("a")),
            Token::Comma,
            Token::Name(String::from("b")),
            Token::RightBracket,
            Token::Arrow,
        ]
    );
    assert_eq!(lines[1].as_ref().unwrap().indentation, 3);
    assert_eq!(lines[1].as_ref().unwrap().number, 2);
    assert_eq_vec!(
        lines[1].as_ref().unwrap().tokens,
        vec![
            Token::Name(String::from("prod")),
            Token::Assignment,
            Token::Name(String::from("a")),
            Token::Operation(Op::Multiplication),
            Token::Name(String::from("b")),
        ]
    );
    assert_eq!(lines[2].as_ref().unwrap().indentation, 3);
    assert_eq!(lines[2].as_ref().unwrap().number, 3);
    assert_eq_vec!(
        lines[2].as_ref().unwrap().tokens,
        vec![Token::Name(String::from("prod")),]
    );
}

#[test]
fn test_cons() {
    let input = "  list=cons(1, cons(2, cons(3, nil)))";
    let lines = lines(input.as_bytes()).collect::<Vec<_>>();
    assert_eq!(lines[0].as_ref().unwrap().indentation, 2);
    assert_eq!(lines[0].as_ref().unwrap().number, 1);
    assert_eq_vec!(
        lines[0].as_ref().unwrap().tokens,
        vec![
            Token::Name(String::from("list")),
            Token::Assignment,
            Token::Cons,
            Token::LeftBracket,
            Token::Number(String::from("1")),
            Token::Comma,
            Token::Cons,
            Token::LeftBracket,
            Token::Number(String::from("2")),
            Token::Comma,
            Token::Cons,
            Token::LeftBracket,
            Token::Number(String::from("3")),
            Token::Comma,
            Token::Nil,
            Token::RightBracket,
            Token::RightBracket,
            Token::RightBracket,
        ]
    );
}
