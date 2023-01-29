import "lib.ls"

genNaturalsFrom = [n] ->
    cons(n, genNaturalsFrom(n + 1))

print(length(take(50, genNaturalsFrom(5))))

fibList = cons(0, cons(1, zipMap([x, y] -> x + y, zip(fibList, right(fibList)))))
print(take(10, fibList))

factorialList = (cons(1, zipMap([x, y] -> x * y, zip(factorialList, genNaturalsFrom(2)))))
print(take(10, factorialList))

print(take(10, filter([x] -> (x % 2) == 0, genNaturalsFrom(0))))
