main :: IO()
main = do
    print $ isGraceful t1 == True
    print $ isGraceful t2 == False

    print $ isGraceful t3 == True
    print $ isGraceful nilTree == True
    --my tests
    



isGraceful :: NTree -> Bool
isGraceful Nil = True
isGraceful t = all even (getNodes t) || all odd (getNodes t) 

getNodes :: NTree -> [Int]
getNodes Nil = []
getNodes (Node value children) = value : concatMap getNodes children


data NTree = Nil | Node Int [NTree]
 deriving (Show)

t1 :: NTree
t1 = Node 1 [Node 3 [Nil], Node 5 [Nil], Node 7 [Nil], Node 9 [Nil]]

t2 :: NTree
t2 = Node 7 [Node 9 [Node 5 [Nil], Node 2 [Nil]]]

t3 :: NTree
t3 = Node 1 [Node 3 [Node 5 [Node 15 [Nil]], Node 9 [Nil], Node 11 [Nil]], Node 7 [Node 13 [Nil]]]

nilTree :: NTree
nilTree = Nil