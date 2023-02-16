# lambda-script
Course project for Programming with Rust:  
Interpreter for Custom Functional Programming language with lazy evaluation

## Some code examples
```
length = [list] -> if empty(list) then 0 else 1 + length(right(list))

take = [n, list] ->
    if (n == 0) | empty(list) then
        nil
    else 
        cons(left(list), take(n - 1, right(list)))
```

```
genNaturalsFrom = [n] ->
    cons(n, genNaturalsFrom(n + 1))

print(take(5, genNaturalsFrom(3)))
```
prints to stdout
```
(3, (4, (5, (6, (7, nil)))))
```

## Running
 - ```cargo run --release -- <filename>```
 - or as executable ```lambda-script <filename>```

# Short description

## Expressions
**Expressions** are lazily evaluated by default. Expressions are:
 - **values**:
   - Function
   - Boolean
   - Number - internally represented as 32 bit integer or 64 bit float
   - Char - single unicode character
   - Tuple(Value, Value)
   - Nil
 - **names** - aliases of other names or expressions
 - **function calls**
 - **if then else expression**
 - **unary operation**: ```-``` and ```!```
 - **binary operation**: ```& | ^ + - / // * ** % == != < > <= >= ```
 - **expression in brackets**: ```(<expression>)```

## Functions
**Functions** are first class objects and can be two types:
- **Pure** (default): have assignments and only one expression. Calling impure functions in the body of pure function (pure scope) will result in error. Lazy evaluation in pure functions is guaranteed.
- **Impure**: can have multiple expressions each evaluated eagerly. Print and read calls are impure. The outside scope is also impure.

Built-in functions:
 - ```read``` reads line from the std in and returns list of characters. The only valid place for this expression is in assignment.
 - ```print(<expr>)```  prints value to std out
 - ```left(<expr>)``` if expression is cons or tuple, returns left element
 - ```right(<expr>)``` if expression is cons or tuple, returns right element
 - ```cons(<expr>, <expr>)``` constructs new tuple from the expressions
 - ```empty(<expr>)``` checks if expression is cons or nil

## Imports
**Imports** are also available. If the import statement is in the scope of pure function, then The imported file outside scope should consist only of assignments. If the import statement is in impure scope, then there are no restrictions.
 - ``` import once ``` imports the file only if it has not been imported in the current scope. It prevents circular imports.
```
import "example.ls"
```
```
import once "lib.ls"
```
## Comments
Symbol # marks beginning of a **comment**. Comments end at the end of the line.
Example:
```
# This is a comment
print(2 + 5) #This is another comment
```

## Operators
- `+` `-` `*` `%` `==` `!=` `<` `>` `<=` `>=` have the expected behavior   
- `!` `&` `|` `^` - are the boolean **negation**, **and**, **or** and **xor**   
- `/` is floating point **division**  
- `//`is **integer** **division**  
- `**` is **exponentiation**  
- Arithmetic operations on numbers will cast their inner representation to floats if necessary.  
- **char** +/- **char** results in **number**
- **number** +/- **char** results in **number**
- **char** +/- **number** results in **char**

# Syntax

## Scope
One or more lines with the same indentation are called a scope. The scopes are two types Pure and Impure.
 - **Pure** scope can have multiple assignments but has to end with a single expression. This will be the expression that the scope evaluates to.
 - **Impure** scope can have multiple assignments and expressions in arbitrary order. If the scope ends with an expression, this will be the expression that the scope evaluates to, else the scope will evaluate to Nil.

The global scope - the lines without indentation - is impure
## Assignments
```
<name> = <expression>
<name> = read
```
## Expressions

### **Functions**
Functions are values and their syntax is:
 - optional modifier ```impure```
 - optional arbitrary number of **parameters** - names in box brackets separated with commas ```[<name>, <name>, ...]```
 - arrow ```->```
 - - **expression** on the same line or 
 - - **scope** starting on a new line with bigger indentation than the current line.

The scope is either pure or impure according to the ```impure``` modifier.
```
-> <expression>

-> 
    <pure scope>

[<name>, <name>, ...] -> <expression>

[<name>, <name>, ...] ->
    <pure scope>
```
```
impure -> <expression>

impure ->
    <impure scope>

impure [<name>, <name>, ...] -> <expression>

impure [<name>, <name>, ...] ->
    <impure scope>
```
Note that the **functions** are **values** and has to be assigned to a name in order to be used.

### **If else expressions**
If else expression syntax is: 
 - ```if```
 - **condition** expression
 - ```then```
 - **expression** or indented **scope** on the next line
 - ```else```  - If the previous was scope must be on the next line with the same indentation as the ```if```
 - **expression** or indented **scope** on the next line

The type of scopes depend on the type of the outside scope.
```
if <expression> then <expression> else <expression>

if <expression> then <expression>
else <expression>

if <expression> then <expression>
else
    <scope>

if <expression> then
    <scope> 
else <expression>

if <expression> then
    <scope> 
else 
    <scope>
```
**Nesting** is possible:
```
if <expression> then
    <scope>
else if <expression> then
    <scope>
else
    <scope>
```

### **Function calls**
Function calls syntax consists of:
 - **expression** that must (eagerly) evaluate to **function**
 - arbitrary number of **arguments** - expressions
```
<expr>(<expr>, <expr>, ..., <expr>)
```

### **Literals**
Supported literals are:
 - Characters: ```'a'```, ```'4'```
 - Strings: ```"apple"```, ```""```
 - Numbers: ```1234``` or ```12.34```
