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

genNaturalsFromTo = [n, m] ->
    if n == m then
        nil
    else
        cons(n, genNaturalsFromTo(n + 1, m))

print(takeFirst(1, genNaturalsFromTo(2, 4)))
