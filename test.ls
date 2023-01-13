length = [list] ->
    if empty(list) then
        0
    else 
        1 + length(right(list))

take = [n, list] ->
    if n == 0 then
        nil
    else 
        cons(left(list), take(n - 1, right(list)))

genNaturalsFrom = [n] ->
    cons(n, genNaturalsFrom(n + 1))

map = [function, list] ->
    if empty(list) then
        nil
    else
        cons(function(left(list)), map(function, right(list)))

invert = [x] ->
    -x

finiteList = cons(1, cons(2, cons(3, cons(4, cons(5, cons(6, cons(7, nil)))))))
infiniteList = genNaturalsFrom(1)

print(take(5, map(invert, infiniteList)))
print(take(5, map(invert, finiteList)))
