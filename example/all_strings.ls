import once "lists.ls" # map, cat, printlnList, forEach
import once "numbers.ls" # parseNumber

allBinaryStrings = [n] ->
    if n == 1 then cons(cons(0, nil), cons(cons(1, nil), nil)) 
    else
        rest = allBinaryStrings(n - 1)
        first = map([xs] -> cons(0, xs), rest)
        second = map([xs] -> cons(1, xs), rest)
        cat(first, second)

print("What k to use for all strings with length k? ")
input = read()
k = parseNumber(input)

forEach(printlnList, allBinaryStrings(k))
