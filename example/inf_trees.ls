import "lib.ls"

-- binary tree constructor
nodeCons = [data, leftChild, rightChild] ->
    cons(data, cons(leftChild, cons(rightChild, nil)))

-- getters
data = [node] -> nth(0, node)
leftChild = [node] -> nth(1, node)
rightChild = [node] -> nth(2, node)

-- produces lazy list that traverses the tree with bfs
bfsList = [tree] ->
    traverse = [queue] ->
        if empty(queue) then -- return
            nil
        else
            tree = left(queue)
            if empty(tree) then -- skip
                traverse(right(queue))
            else
                newQueue = cat(right(queue), cons(leftChild(tree), cons(rightChild(tree), nil)))
                cons(data(tree), traverse(newQueue))
    traverse(cons(tree, nil))

-- testing
genNaturals = ->
    genNaturalsFrom = [n] ->
        cons(n, genNaturalsFrom(n+1))
    genNaturalsFrom(0)

-- splits the list into two lists: the first consists the elements at even positions and the second the rest
-- returns a tuple of lists
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

makeIntoTree = [list] ->
    if empty(list) then nil
    else
        split = splitList(right(list))
        nodeCons(left(list), makeIntoTree(left(split)), makeIntoTree(right(split)))

print(take(10, bfsList(makeIntoTree(genNaturals()))))
