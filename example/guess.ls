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
