main :: IO()
main = do
    print $ convert tree == Node 30 (Node 36 (Node 36 Nil Nil) (Node 35 Nil (Node 33 Nil Nil))) (Node 21 (Node 26 Nil Nil) (Node 15 Nil (Node 8 Nil Nil)))
    print $ convert secondtree == Node 32 (Node 40 (Node 41 Nil Nil) (Node 35 Nil Nil)) (Node 24 Nil (Node 14 Nil Nil))
    -- my test
    print $ convert niltree == Nil
    --my test
convert :: Btree -> Btree
convert Nil = Nil
convert (Node value left right) = helper (Node value left right) 0
 where
    helper :: Btree -> Int -> Btree
    helper Nil _ = Nil
    helper (Node value left right) acc = let newNodeValue  =  value + sumOfNodes right + acc
                                    in  Node newNodeValue (helper left newNodeValue) (helper right acc)  

sumOfNodes :: Btree -> Int
sumOfNodes Nil = 0
sumOfNodes (Node value left right) = sumOfNodes left + value + sumOfNodes right


tree :: Btree
tree = Node 4 (Node 1 (Node 0 Nil Nil) (Node 2 Nil (Node 3 Nil Nil))) (Node 6 (Node 5 Nil Nil) (Node 7 Nil (Node 8 Nil Nil)))

secondtree :: Btree
secondtree = Node 8 (Node 5 (Node 1 Nil Nil) (Node 3 Nil Nil)) (Node 10 Nil (Node 14 Nil Nil))

niltree :: Btree
niltree = Nil

data Btree = Nil | Node Int Btree Btree
 deriving (Eq, Show)
