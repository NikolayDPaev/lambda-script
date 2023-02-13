import "numbers.ls"

actual = 12
guess = impure ->
    input = read
    num = parseNumber(input)
    if actual == num then
        print("Success")
    else if actual < num then
        print("Lower")
        guess()
    else 
        print("Higher")
        guess()

print("Guess num")
guess()
