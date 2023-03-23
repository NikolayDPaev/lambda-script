# Example:
import once "numbers.ls" as nums # parseNumber
import once "lists.ls" # cat

returnMessage = [number] ->
    if number == 42 then
        "Correct!"
    else
        "Error!"

println(cat("What is the answer to the ", "Ultimate Question of Life, the Universe, and Everything?"))
input = read()
parsedInput = nums.parseNumber(input)

println(returnMessage(parsedInput))
