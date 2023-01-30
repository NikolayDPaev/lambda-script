# lambda-script
Course project for Programming with Rust:  
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
outputs:
```
(0, (1, (1, (2, (3, nil)))))
```

# Very short description

## Expressions:
**Expressions** are lazily evaluated by default. Expressions are:
 - **values**: Function, Boolean, Number, Char, Tuple(Value, Value), Nil
 - **names**
 - **function calls**
 - **if else expression**
 - **unary operation**: ```-``` and ```!```
 - **binary operation**: ```& | ^ + - / // * ** % == != < > <= >= ```
 - **expression in brackets**: ```(<expression>)```

## Functions
**Functions** are first class objects and can be two types:
- **Pure** (default): have assignments and only one expression. Calling nonpure functions in pure scope will result in error. Lazy evaluation in pure functions definition is guaranteed.
- **Nonpure**: can have multiple expressions each evaluated eagerly. Print and read calls are nonpure. The outside scope is also non pure.

Built-in functions:
 - ```read``` reads line from the std in and returns list of characters. The only valid place for this expression is in assignment.
 - ```print(<expr>)```  prints value to std out
 - ```left(<expr>)``` if expression is cons or tuple, returns left element
 - ```right(<expr>)``` if expression is cons or tuple, returns right element
 - ```cons(<expr>, <expr>)``` constructs new tuple from the expressions
 - ```empty(<expr>)``` checks if expression is cons or nil

## Imports
**Imports** are also available. The imported files should consist only of assignments at the top level - like libraries.
Note that the interpreter does not check for circular imports
```
import "example.ls"
```
## Comments
Symbol # marks beginning of a **comment**. Comments end at the end of the line.
Example:
```
# This is a comment
print(2 + 5) # This is another comment
```

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
<name> = read
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
**Nesting** is possible:
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
