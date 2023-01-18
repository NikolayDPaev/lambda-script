tailRecFib = [n] ->
    helper = [n, a, b] ->
        if n == 0 then b
        else
            helper(n - 1, b, a + b)
    if n == 0 then 0
    else if n == 1 then 1
    else helper(n - 1, 0, 1)

print(tailRecFib(10))
