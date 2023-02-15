import once "numbers.ls"

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

print("Guess number:")
guess()

if true then
    # test has been imported in the outside scope
    # so it should not be imported again
    import once "test.ls" # should not execute
else nil
