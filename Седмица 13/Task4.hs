import Data.List
main :: IO()
main = do
    print $ leavesAreEqual t1 t2 == True
    print $ leavesAreEqual t1 t4 == False
    print $ leavesAreEqual t1 nilTree == False
    --my test

leavesAreEqual :: Btree -> Btree -> Bool
leavesAreEqual bt1 bt2 = (sort $ traverseDFS bt1) == (sort $ traverseDFS bt2)

traverseDFS :: Btree -> [Int]
traverseDFS Nil = []
traverseDFS (Node value left right) = traverseDFS left ++ [value] ++ traverseDFS right
 
t1 :: Btree
t1 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 26 Nil Nil) (Node 32 Nil Nil)))

t2 :: Btree
t2 = Node 25 (Node 10 (Node 1 Nil Nil) Nil) (Node 30 (Node 32 Nil Nil) (Node 26 Nil Nil))

t4 :: Btree
t4 = Node 10 (Node 1 Nil Nil) (Node 25 Nil (Node 30 (Node 27 Nil Nil) (Node 32 Nil Nil)))

nilTree :: Btree
nilTree = Nil


data Btree = Nil | Node Int Btree Btree
