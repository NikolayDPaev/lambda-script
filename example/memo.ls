fib = [n] ->
    if n == 0 then 0
    else if n == 1 then 1
    else fib(n-1) + fib(n-2)

# println(fib(25))

iterFib = [n] ->
    helper = [n, a, b] ->
        if n == 0 then b
        else
            helper(n - 1, b, a + b)
    if n == 0 then 0
    else if n == 1 then 1
    else helper(n - 1, 0, 1)

checkFib = [n] ->
    loop = [m] ->
        fib = iterFib(m)
        if n == fib then true
        else if n < fib then false
        else loop(m + 1)
    loop(0)

println(checkFib(102334155))
