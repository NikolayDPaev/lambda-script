import once "numbers.ls"

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

# Example of cyclic import detection
# if true then
#     # test has been imported in the outside scope
#     # so it should not be imported again
#     import once "test.ls" # should not execute
# else nil
