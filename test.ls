length = [list] ->
    if empty(list) then
        0
    else 
        1 + length(right(list))

take = [n, list] ->
    if (n == 0) | empty(list) then
        nil
    else 
        cons(left(list), take(n - 1, right(list)))

genNaturalsFrom = [n] ->
    cons(n, genNaturalsFrom(n + 1))

map = [function, list] ->
    if empty(list) then
        nil
    else
        head = function(left(list))
        tail = map(function, right(list))
        cons(head, tail)

zip = [list1, list2] ->
    if empty(list1) | empty(list2) then
        nil
    else 
        cons(cons(left(list1), left(list2)), zip(right(list1), right(list2)))

plus = [x, y] ->
    x + y

multiply = [x, y] ->
    x * y

zipMap = [function, list] ->
    if empty(list) then
        nil
    else
        head = function(left(left(list)), right(left(list)))
        tail = zipMap(function, right(list))
        cons(head, tail)

fibList = cons(0, cons(1, zipMap(plus, zip(fibList, right(fibList)))))
print(take(10, fibList))

factorialList = (cons(1, zipMap(multiply, zip(factorialList, genNaturalsFrom(2)))))
print(take(10, factorialList))
