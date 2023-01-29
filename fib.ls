tailRecFib = [n] ->
    helper = [n, a, b] ->
        if n == 0 then b
        else
            helper(n - 1, b, a + b)
    if n == 0 then 0
    else if n == 1 then 1
    else helper(n - 1, 0, 1)

# print(tailRecFib(35))

addTo = [n] ->
    if n == 0 then 0
    else n + addTo(n - 1)

#print(addTo(10))

square = [n] -> n*n

test = ->
    square(1 + 2)  

#print(test())

import "lib.ls"

genNaturalsFrom = [n] ->
    cons(n, genNaturalsFrom(n + 1))

#print(take(10, genNaturalsFrom(5)))

actual = 'f'
guess = nonpure ->
    input = read
    input_char = left(input)
    if actual == input_char then
        print("Success")
    else if actual < input_char then
        print("Lower")
        guess()
    else 
        print("Higher")
        guess()

print("Guess char")
guess()
