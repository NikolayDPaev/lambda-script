import "lib.ls"

genNaturalsFrom = [n] ->
    cons(n, genNaturalsFrom(n + 1))

print(take(5, genNaturalsFrom(3)))
 
fibList = ->
    cons(0, cons(1, zipMap([x, y] -> x + y, zip(fibList(), right(fibList())))))
    
factorialList = ->
    (cons(1, zipMap([x, y] -> x * y, zip(factorialList(), genNaturalsFrom(2)))))

print(take(10, fibList()))
print(take(10, factorialList()))

print(take(10, filter([x] -> (x % 2) == 0, genNaturalsFrom(0))))
