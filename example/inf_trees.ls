import once "lists.ls" # nth, printlnList, take

# binary tree constructor
nodeCons = [data, leftChild, rightChild] ->
    cons(data, cons(leftChild, cons(rightChild, nil)))

# getters
data = [node] -> nth(0, node)
leftChild = [node] -> nth(1, node)
rightChild = [node] -> nth(2, node)

# produces lazy list that traverses the tree with bfs
bfsList = [tree] ->
    traverse = [queue] ->
        if empty(queue) then # return
            nil
        else
            tree = left(queue)
            if empty(tree) then # skip
                traverse(right(queue))
            else
                newQueue = cat(right(queue), cons(leftChild(tree), cons(rightChild(tree), nil)))
                cons(data(tree), traverse(newQueue))
    traverse(cons(tree, nil))

# testing

makeNatsIntoTree = [n] ->
    nodeCons(n, makeNatsIntoTree(1 + 2*n), makeNatsIntoTree(2 + 2*n))

printlnList(take(15, bfsList(makeNatsIntoTree(0))))
