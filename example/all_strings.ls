import once "lists.ls" as ls # map, cat, printlnList, forEach
import once "numbers.ls" as nums # parseNumber

allBinaryStrings = [n] ->
    if n == 1 then cons(cons(0, nil), cons(cons(1, nil), nil)) 
    else
        rest = allBinaryStrings(n - 1)
        first = ls.map([xs] -> cons(0, xs), rest)
        second = ls.map([xs] -> cons(1, xs), rest)
        ls.cat(first, second)

print("What k to use for all strings with length k? ")
input = read()
k = nums.parseNumber(input)

ls.forEach(ls.printlnList, allBinaryStrings(k))
