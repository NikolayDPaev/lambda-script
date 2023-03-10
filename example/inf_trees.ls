import once "lists.ls"

# arbitrary tree constructor
nodeCons = [data, children] ->
    cons(data, children)

# getters
data = [node] -> nth(0, node)
nthChild = [node, n] -> nth(n + 1, node)

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
                newQueue = cat(right(queue), right(tree))
                cons(data(tree), traverse(newQueue))
    traverse(cons(tree, nil))

# ----------------------------------------------------------------

makeFloatsTree = [] ->
    helper = [n, depth] ->
        coeff = 10 ** depth
        children = map([x] -> helper(n + (x / coeff), depth + 1), range(1, 10))
        nodeCons(n, children)
    helper(0, 1)

makeNatsTree = [] ->
    helper = [n] ->
        nodeCons(n, cons(helper(1 + 2*n), cons(helper(2 + 2*n), nil)))
    helper(0)

# joking of course, cannot count the real numbers
countReals = [] ->
    infFloats = bfsList(makeFloatsTree())
    infNats = bfsList(makeNatsTree())
    zip(infNats, infFloats)

printlnList(take(30, countReals()))
