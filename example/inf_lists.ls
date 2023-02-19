import once "lists.ls"

genNaturalsFrom = [n] ->
    cons(n, genNaturalsFrom(n + 1))

println(take(5, genNaturalsFrom(3)))
 
fibList = ->
    cons(0, cons(1, zipMap([x, y] -> x + y, zip(fibList(), right(fibList())))))
    
factorialList = ->
    (cons(1, zipMap([x, y] -> x * y, zip(factorialList(), genNaturalsFrom(2)))))

println(take(10, fibList()))
println(take(10, factorialList()))

println(take(10, filter([x] -> (x % 2) == 0, genNaturalsFrom(0))))
