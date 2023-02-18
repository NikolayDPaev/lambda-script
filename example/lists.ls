# List functions
length = [list] -> if empty(list) then 0 else 1 + length(right(list))

map = [f, list] ->
    if empty(list) then
        nil
    else
        head = f(left(list))
        tail = map(f, right(list))
        cons(head, tail)

take = [n, list] ->
    if (n == 0) | empty(list) then
        nil
    else 
        cons(left(list), take(n - 1, right(list)))

nth = [n, list] ->
    if empty(list) then
        nil
    else if n == 0 then
        left(list)
    else
        nth(n - 1, right(list))

last = [list] ->
    if empty(list) then nil
    else if empty(right(list)) then
        left(list)
    else 
        last(right(list))

zip = [list1, list2] ->
    if empty(list1) | empty(list2) then
        nil
    else cons(cons(left(list1), left(list2)), zip(right(list1), right(list2)))

zipMap = [f, list] ->
    if empty(list) then nil
    else
        head = f(left(left(list)), right(left(list)))
        tail = zipMap(f, right(list))
        cons(head, tail)

filter = [predicate, list] ->
    if empty(list) then
        nil
    else
        head = left(list)
        if predicate(head) then
            cons(head, filter(predicate, right(list)))
        else
            filter(predicate, right(list))

foldRight = [f, init, list] ->
    if empty(list) then init
    else 
        f(left(list), foldRight(f, init, right(list)))

foldLeft = [f, init, list] ->
    if empty(list) then init
    else
        foldLeft(f, f(left(list), init), right(list))

cat = [xs, ys] ->
    if empty(xs) then ys
    else cons(left(xs), cat(right(xs), ys))
