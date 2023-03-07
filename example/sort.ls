import "lists.ls"
import "numbers.ls"

merge = [x, y] ->
    if empty(x) then y
    else if empty(y) then x
    else if left(x) < left(y) then cons(left(x), merge(right(x), y))
    else cons(left(y), merge(x, right(y)))

splitList = [list] ->
    if empty(list) then
        cons(nil, nil)
    else if empty(right(list)) then
        first = cons(left(list), nil)
        second = nil
        cons(first, second)
    else
        rest = splitList(right(right(list)))
        first = cons(left(list), left(rest))
        second = cons(left(right(list)), right(rest)) 
        cons(first, second)

mergesort = [list] ->
    if empty(list) then nil
    else if empty(right(list)) then list
    else if empty(right(right(list))) then
        if left(list) > left(right(list)) then
            cons(left(right(list)), cons(left(list), nil))
        else
            cons(left(list), cons(left(right(list)), nil))
    else
        split = splitList(list)
        merge(mergesort(left(split)), mergesort(right(split)))

selectionsort = [list] ->
    if empty(list) then list
    else
        maxElement = foldLeft(max, left(list), right(list))
        withoutMax = filter([x] -> x != maxElement, list)
        rest = selectionsort(withoutMax)
        cons(maxElement, rest)


list = range(0, 100)
# printlnList(selectionsort(list))
printlnList(mergesort(list))
