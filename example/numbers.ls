-- Number functions

-- min, max, abs
min = [a, b] -> if a < b then a else b
max = [a, b] -> if a > b then a else b
abs = [n] -> if n < 0 then -n else n

-- parses a string to number
parseNumber = [string] ->
    -- string - the rest of the string
    -- n - accumulated number by far
    -- digitsAfterPoint - the number of digits after the decimal point
    parse = [string, n, digitsAfterPoint] ->
        if empty(string) then
            if digitsAfterPoint > 0 then n / (10 ** (digitsAfterPoint - 1))
            else n
        else
            head = left(string)
            if head == '.' then 
                parse(right(string), n, 1)
            else if ((head - '0') < 10) & ((head - '0') >= 0) then
                digit = (left(string) - '0')
                if digitsAfterPoint > 0 then
                    parse(right(string), (n * 10) + digit, digitsAfterPoint + 1)
                else
                    parse(right(string), (n * 10) + digit, digitsAfterPoint)
            else 
                nil
    
    if empty(string) then
        nil
    else if left(string) == '-' then
        -parse(right(string), 0, 0)
    else 
        parse(string, 0, 0)

-- parses a number as integer to a string
intToString = [n] -> 
    helper = [n, string] ->
        if n == 0 then string
        else
            char = '0' + (n % 10)
            helper(n // 10, cons(char, string))
    
    if n == 0 then "0"
    else if n < 0 then
        cons('-', helper(abs(n), nil))
    else helper(n, nil)
