import once "lists.ls" # range, filter, applyOnUserInput
import once "numbers.ls" # min

isPrime = [n] ->
    sqrtn = n ** (1/2)
    rangeToCheck = range(2, min(n, sqrtn + 1))
    loop = [range] ->
        if empty(range) then true
        else if n % left(range) == 0 then false
        else loop(right(range))
    loop(rangeToCheck)

infPrimes = -> filter(isPrime, infiniteRange(2))

print("Press enter for next:")
applyOnUserInput(print, infPrimes())
