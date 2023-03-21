import once "numbers.ls" # parseNumber
import once "lists.ls" # length, map, foldLeft

linRegression = [points] ->
    n = length(points)
    xs = map([point] -> left(point), points)
    ys = map([point] -> right(point), points)

    sum_x = foldLeft([x, acc] -> x + acc, 0, xs)
    sum_y = foldLeft([y, acc] -> y + acc, 0, ys)
    sum_x_squared = foldLeft([x, acc] -> x**2 + acc, 0, xs)
    sum_xy = foldLeft([point, acc] -> acc + (left(point) * right(point)), 0, points)

    mean_x = sum_x / n
    mean_y = sum_y / n
    
    a = (sum_xy - sum_x * mean_y) / (sum_x_squared - sum_x * mean_x)
    b = mean_y - a * mean_x
    cons(a, b)

main = impure ->
    print("Enter number of points: ")
    n_str = read()
    n = parseNumber(n_str)

    readTuple = impure ->
        print("x: ")
        x_str = read()
        print("y: ")
        y_str = read()
        cons(parseNumber(x_str), parseNumber(y_str))

    readList = impure [n] ->
        if n == 0 then nil
        else 
            cons(readTuple(), readList(n - 1))

    list = readList(n)
    result = linRegression(list)

    print("a = ")
    println(left(result))
    print("b = ")
    println(right(result))
    
main()
