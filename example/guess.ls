import once "numbers.ls" # parseNumber

actual = 12
guess = impure ->
    input = read
    num = parseNumber(input)
    if actual == num then
        println("Success")
    else if actual < num then
        println("Lower")
        guess()
    else 
        println("Higher")
        guess()

print("Guess number: ")
guess()
