import "lib.ls"

num = parseNumber(read)

fibList = cons(0, cons(1, zipMap([x, y] -> x + y, zip(fibList, right(fibList)))))
print(take(num, fibList))
