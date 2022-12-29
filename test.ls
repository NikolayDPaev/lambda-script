sum = [a, b, c] ->
    a + b + c

main = unpure ->
    c = read()
    result = sum(2, 3, c)
    print(result)
