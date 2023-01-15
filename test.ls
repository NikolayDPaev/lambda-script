import "lib.ls"

genNaturalsFrom = [n] ->
    cons(n, genNaturalsFrom(n + 1))

print(length(take(5, genNaturalsFrom(5))))

fibList = cons(0, cons(1, zipMap([x, y] -> x + y, zip(fibList, right(fibList)))))
print(take(5, fibList))

factorialList = (cons(1, zipMap([x, y] -> x * y, zip(factorialList, genNaturalsFrom(2)))))
print(take(10, factorialList))

if (5.5 - 3.5) == 2 then print(true) else print(false)
