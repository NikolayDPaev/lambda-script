import once "lists.ls"

numberPrompt = impure ->
    # imports only in the current scope
    import once "numbers.ls"
    println("How many fibs do you want?")
    input = read
    parseNumber(input)

num = numberPrompt()

fibList = -> cons(0, cons(1, zipMap([x, y] -> x + y, zip(fibList(), right(fibList())))))
println(take(num, fibList()))

allStrings = [n] ->
    if n == 1 then cons(cons(0, nil), cons(cons(1, nil), nil))
    else
        rest = allStrings(n - 1)
        first = map([xs] -> cons(0, xs), rest)
        second = map([xs] -> cons(1, xs), rest)
        cat(first, second)


# has not been imported yet in this scope
import once "numbers.ls"
println("What k to use for all strings with length k?")
input = read
num = parseNumber(input)
println(allStrings(num))

import "guess.ls"
println("One more game?")
import "guess.ls"
