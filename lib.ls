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

parseNumber = [list] ->
    parse = [list, n] ->
        if empty(list) then
            n
        else
            head = left(list)
            if ((head - '0') < 10) & ((head - '0') >= 0) then
                parse(right(list), (n * 10) + (left(list) - '0'))
            else 
                nil
    
    if empty(list) then
        nil
    else if left(list) == '-' then
        -parse(right(list), 0)
    else 
        parse(list, 0)
