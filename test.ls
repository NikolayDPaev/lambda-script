import "lib.ls"

genNaturalsFrom = [n] ->
    cons(n, genNaturalsFrom(n + 1))

plus = [x, y] -> между другото, тука мога да си пиша каквото си искам
    x + y

multiply = [x, y] ->
    x * y

invert = [x] ->
    -x

fibList = cons(0, cons(1, zipMap(plus, zip(fibList, right(fibList)))))
print(take(parseNumber(read), fibList))

factorialList = (cons(1, zipMap(multiply, zip(factorialList, genNaturalsFrom(2)))))
print(take(10, factorialList))
