length = [list] ->
    if list == nil then
        0
    else 
        1 + length(right(list))

takeFirst = [n, list] ->
    if n == 0 then
        nil
    else 
        cons(left(list), takeFirst(n - 1, right(list)))

genNaturalsFrom = [n] ->
    cons(n, genNaturalsFrom(n + 1))

print(takeFirst(5, genNaturalsFrom(1)))
