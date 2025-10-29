main :: IO ()
main = do
    print $ rangedSum firstTree 100 50 == 0 -- (L = 100, R = 50)
    print $ rangedSum firstTree 7 15 == 32 -- (L = 7, R = 15)
    print $ rangedSum firstTree 15 7 == 32 -- (L = 15, R = 7)
    print $ rangedSum secondTree 6 10 == 23 -- (L = 6, R = 10)
    print $ rangedSum secondTree 10 6 == 23 -- (L = 10, R = 6)
    
    print $ rangedSum thirdTree 5 10 == 31
   -- print $ rangedSum nilTree 5 10 -- error Empty tree
    --my tests

rangedSum :: Btree -> Int -> Int -> Int
rangedSum Nil _ _ = error "Empty tree!"
rangedSum t bound1 bound2 = sum $ [x | x <- getNodesDFS t, x >= min bound1 bound2, x <= max bound1 bound2]


getNodesDFS :: Btree -> [Int]
getNodesDFS Nil = []
getNodesDFS (Node value left right) = getNodesDFS left ++ [value] ++ getNodesDFS right 

firstTree :: Btree
firstTree = Node 10 (Node 5 (Node 3 Nil Nil) (Node 7 Nil Nil)) (Node 15 Nil (Node 18 Nil Nil))

secondTree :: Btree
secondTree = Node 10 (Node 5 (Node 3 (Node 1 Nil Nil) Nil) (Node 7 (Node 6 Nil Nil) Nil)) (Node 15 (Node 13 Nil Nil) (Node 18 Nil Nil))

nilTree :: Btree
nilTree = Nil

thirdTree :: Btree
thirdTree = Node 4 (Node 5 (Node 1 Nil Nil) (Node 13 (Node 9 Nil Nil) Nil)) (Node 7 (Node 2 Nil Nil) (Node 10 Nil Nil))
data Btree = Nil | Node Int Btree Btree