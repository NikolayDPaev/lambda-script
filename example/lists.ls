# List functions

# pure functions
foldRight = [f, init, list] ->
    if empty(list) then init
    else 
        f(left(list), foldRight(f, init, right(list)))

foldLeft = [f, init, list] ->
    if empty(list) then init
    else
        foldLeft(f, f(left(list), init), right(list))

length = [list] ->
   foldLeft([x, acc] -> 1 + acc, 0, list)

map = [f, list] ->
    # no substitution errors because the evaluator
    # work with unique identifiers, not names
    foldRight([x, list] -> cons(f(x), list), nil, list)

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
    foldRight([tuple, list] -> cons(f(left(tuple), right(tuple)), list), nil, list)

filter = [p, list] ->
    foldRight([x, list] -> if p(x) then cons(x, list) else list, nil, list)

cat = [xs, ys] ->
    foldRight([x, acc] -> cons(x, acc), ys, xs)

cmp = [s1, s2] ->
    if empty(s1) & empty(s2) then true
    else if empty(s1) | empty(s2) then false
    else if left(s1) != left(s2) then false
    else
        cmp(right(s1), right(s2))

cmpOrdered = [s1, s2] ->
    if empty(s1) & empty(s2) then 0
    else if empty(s1) then -1
    else if empty(s2) then 1
    else if left(s1) < left(s2) then -1
    else if left(s1) > left(s2) then 1
    else cmpOrdered(right(s1), right(s2))

tokenize = [str, delimiter] ->
    if empty(str) then cons(nil, nil)
    else if left(str) == delimiter then
        cons(nil, tokenize(right(str), delimiter))
    else 
        rest = tokenize(right(str), delimiter)
        cons(cons(left(str), left(rest)), right(rest))

range = [a, b] ->
    if a >= b then nil
    else cons(a, range(a + 1, b))

infiniteRange = [a] ->
    cons(a, infiniteRange(a + 1))

# impure functions
printlnList = impure [xs] ->
    helper = impure [xs] ->
        if empty(xs) then
            println(']')
        else if empty(right(xs)) then
            print(left(xs))
            println(']')
        else
            print(left(xs))
            print(", ")
            helper(right(xs))
    print('[')
    helper(xs)

forEach = impure [f, xs] ->
    if empty(xs) then
        nil
    else
        f(left(xs))
        forEach(f, right(xs))
