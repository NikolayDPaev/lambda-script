use std::path::PathBuf;

use crate::Interpreter;

#[test]
fn test_infinite_gen() {
    let script = "take = [n, list] ->
    if (n == 0) | empty(list) then
        nil
    else
        cons(left(list), take(n - 1, right(list)))

genNaturalsFrom = [n] ->
    cons(n, genNaturalsFrom(n + 1))

print(take(3, genNaturalsFrom(1)))";
    let input = vec![];
    let mut output = Vec::<u8>::new();
    let mut interpreter = Interpreter::new(input.as_slice(), &mut output, false);

    assert_eq!(interpreter.run(script.as_bytes(), PathBuf::new()), 0);
    std::mem::drop(interpreter);
    assert_eq!(
        String::from_utf8(output).unwrap(),
        String::from("(1, (2, (3, nil)))\n")
    );
}

#[test]
fn test_higher_order_function() {
    let script = "map = [f, list] ->
    if empty(list) then
        nil
    else
        head = f(left(list))
        tail = map(f, right(list))
        cons(head, tail)

list = cons(1, cons(2, cons(3, cons(4, cons(5, nil)))))
square = [x] -> x * x

print(map(square, list))";
    let input = vec![];
    let mut output = Vec::<u8>::new();
    let mut interpreter = Interpreter::new(input.as_slice(), &mut output, false);

    assert_eq!(interpreter.run(script.as_bytes(), PathBuf::new()), 0);
    std::mem::drop(interpreter);
    assert_eq!(
        String::from_utf8(output).unwrap(),
        String::from("(1, (4, (9, (16, (25, nil)))))\n")
    );
}

#[test]
fn test_return_higher_order_function() {
    let script = "getFunction = [char] ->
    if char == '+' then
        [x, y] -> x + y
    else if char == '-' then
        [x, y] -> x - y
    else nil

plus = getFunction('+')
print(plus(1, 2))

print(getFunction('-')(2.5, 2))";
    let input = vec![];
    let mut output = Vec::<u8>::new();
    let mut interpreter = Interpreter::new(input.as_slice(), &mut output, false);

    assert_eq!(interpreter.run(script.as_bytes(), PathBuf::new()), 0);
    std::mem::drop(interpreter);
    assert_eq!(String::from_utf8(output).unwrap(), String::from("3\n0.5\n"));
}

#[test]
fn test_guess_char() {
    let script = "
actual = 'f'
guess = impure ->
    input = read
    input_char = left(input)
    if actual == input_char then
        print(\"Success\")
    else if actual < input_char then
        print(\"Lower\")
        guess()
    else 
        print(\"Higher\")
        guess()

print(\"Guess char\")
guess()
";
    let input = String::from("a\nu\nf\n");
    let mut output = Vec::<u8>::new();
    let mut interpreter = Interpreter::new(input.as_bytes(), &mut output, false);

    assert_eq!(interpreter.run(script.as_bytes(), PathBuf::new()), 0);
    std::mem::drop(interpreter);
    assert_eq!(
        String::from_utf8(output).unwrap(),
        String::from("Guess char\nHigher\nLower\nSuccess\n")
    );
}

#[test]
#[cfg_attr(miri, ignore)]
fn test_tail_recursion() {
    let script = "
countTo = [n] ->
    loop = [m, n] ->
        if m == n then m
        else
            loop(m + 1, n)
    loop(0, n)

print(countTo(1000))";
    let input = vec![];
    let mut output = Vec::<u8>::new();
    let mut interpreter = Interpreter::new(input.as_slice(), &mut output, false);

    assert_eq!(interpreter.run(script.as_bytes(), PathBuf::new()), 0);
    std::mem::drop(interpreter);
    assert_eq!(String::from_utf8(output).unwrap(), String::from("1000\n"));
}
