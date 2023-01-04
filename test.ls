sum = [a, b, c] ->
    prod = [a, b] -> 
        a + b
    prod(a, b + c)

a = 55

main = nonpure ->
    c = read()
    result = sum(2, 3, c)
    print(result)

main
