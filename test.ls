length = [list] ->
    if list == nil then
        0
    else 
        1 + length(left(list))

print(length("abcd"))
