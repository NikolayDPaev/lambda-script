import "lib.ls"

num = parseNumber(read)

fibList = cons(0, cons(1, zipMap([x, y] -> x + y, zip(fibList, right(fibList)))))
print(take(num, fibList))

cat = [xs, ys] ->
    if empty(xs) then ys
    else cons(left(xs), cat(right(xs), ys))

allStrings = [n] ->
    if n == 1 then cons(cons(0, nil), cons(cons(1, nil), nil))
    else 
        rest = allStrings(n - 1)
        first = map([xs] -> cons(0, xs), rest)
        second = map([xs] -> cons(1, xs), rest)
        cat(first, second)

print(allStrings(4))

