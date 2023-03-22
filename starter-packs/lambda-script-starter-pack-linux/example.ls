# Example:
import once "lists.ls" # cat
import once "numbers.ls" # parseNumber

returnMessage = [number] ->
    if number == 42 then
        "Correct!"
    else
        "Error!"

println(cat("What is the answer to the ", "Ultimate Question of Life, the Universe, and Everything?"))
input = read()
parsedInput = parseNumber(input)

println(returnMessage(parsedInput))
