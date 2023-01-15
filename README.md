# lambda-script
Course project for Programming with Rust  
Interpreter for Custom Functional Programming language with lazy evaluation

## Some code examples:
```
length = [list] -> if empty(list) then 0 else 1 + length(right(list))

take = [n, list] ->
    if (n == 0) | empty(list) then
        nil
    else 
        cons(left(list), take(n - 1, right(list)))
```

```
fibList = cons(0, cons(1, zipMap([x, y] -> x + y, zip(fibList, right(fibList)))))
print(take(5, fibList))
```
outputs
```
(0, (1, (1, (2, (3, nil)))))
```

# Very short description

## Expressions:
Expressions in pure scope are evaluated lazily. Expressions are:
 - values: Function, Boolean, Number, Char, Tuple(Value, Value), Nil
 - names
 - function calls
 - if else expression
 - unary operation: ```-``` and ```!```
 - binary operation: ```& | ^ + - / // * ** % == != < > <= >= ```
 - ```(<expression>)```

## Functions
Functions are first class objects and can be two types:
- Pure (default): have assignments and only one expression, cannot call nonpure functions, lazy evaluation guaranteed
- Nonpure: can have multiple expressions each evaluated eagerly, print and read calls are nonpure

Built-in functions:
 - ```read``` reads string from the std in
 - ```print(<expr>)```  prints value to std out
 - ```left(<expr>)``` if expression is cons or tuple, returns left element
 - ```right(<expr>)``` if expression is cons or tuple, returns right element
 - ```cons(<expr>, <expr>)``` constructs new tuple from the expressions
 - ```empty(<expr>)``` checks if expression is cons or nil

## Function Syntax:
```
[<name>, <name>, ...] -> <expression>

[<name>, <name>, ...] ->
    <assignment>
    <assignment>
    <assignment>
    ...
    <expression>

[<name>, <name>, ...] ->
    <expression>

nonpure -> <expression>

nonpure ->
    <assignment> or <expression>
    <assignment> or <expression>
    ...
    <expression>

nonpure [<name>, <name>, ...] -> 
    <assignment> or <expression>
    <assignment> or <expression>
    ...
    <expression>
```

## Assignments syntax:
```
<name> = <expression>
```

## If else expressions syntax:
```
if <expression> then <expression> else <expression>

if <expression> then <expression>
else <expression>

if <expression> then
    <expression> 
else <expression>

if <expression> then
    <expression> 
else 
    <expression>

if <expression> then
    <assignment>
    <assignment>
    ...
    <expression> 
else 
    <assignment>
    <assignment>
    ...
    <expression>
```
Nesting is possible:
```
if <expression> then
    <assignment>
    <assignment>
    ...
    <expression>
else if <expression> then
    <assignment>
    <assignment>
    ...
    <expression>
else
    <assignment>
    <assignment>
    ...
    <expression>
```

## Imports
Imports are also available. The imported files should consist only of assignments at the top level - like a libraries.
Note that the interpreter does not check for circular imports
```
import "example.ls"
```
