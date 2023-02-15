import once "lists.ls"

numberPrompt = impure ->
    # imports only in the current scope
    import once "numbers.ls"
    print("How many fibs do you want?")
    input = read
    parseNumber(input)

num = numberPrompt()

fibList = -> cons(0, cons(1, zipMap([x, y] -> x + y, zip(fibList(), right(fibList())))))
print(take(num, fibList()))

allStrings = impure [n] ->
    if n == 1 then cons(cons(0, nil), cons(cons(1, nil), nil))
    else
        rest = allStrings(n - 1)
        first = map([xs] -> cons(0, xs), rest)
        second = map([xs] -> cons(1, xs), rest)
        cat(first, second)


# has not been imported yet in this scope
import once "numbers.ls"
print("What k to use for all strings with length k?")
input = read
num = parseNumber(input)
print(allStrings(num))

import "guess.ls"
print("One more game?")
import "guess.ls"
