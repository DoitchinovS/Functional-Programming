import Data.List
main::IO()
main = do
    print $ listLeaves [(1, 2, 3), (2, 4, 5)] == [4, 3, 5] --the result is [3, 4, 5]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)] == [4, 5, 3]
    print $ listLeaves [(1, 2, 3), (3, 4, 5), (5, 6, 9)] == [2, 4, 6, 9]
    print $ listLeaves [] == []
    --my test
    print $ listLeaves [(1, 2, 3), (2, 4, 5), (3, 6, 7), (5, 8, 9)] == [4, 6, 7, 8, 9]
    --my test

type Node = Int
type Nodes = [Int]
type GraphVector = (Int, Int, Int)
type Graph = [GraphVector]

listLeaves :: Graph -> Nodes
listLeaves graph = [ x | x <- findNodes graph, notElem x (findParents graph)]

findParents :: Graph -> Nodes
findParents = map (\ (a, b, c) -> a)

findNodes :: Graph -> Nodes
findNodes = nub.concatMap (\ (a, b, c) -> [a, b, c])
