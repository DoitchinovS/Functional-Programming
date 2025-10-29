import Data.List
main :: IO()
main = do
   
    print $ maximumLevel tree1 == 2
    print $ maximumLevel tree2 == 3
    print $ maximumLevel tree3 == 3
    --print $ maximumLevel niltree -- error Empty three
    --my test
maximumLevel :: (Num a, Ord a) => BTree a -> Int
maximumLevel Nil = error "Empty tree!"
maximumLevel tree = fst $ foldl1 (\ (accLevel, accSum) (newLevel, newSum) -> if newSum >= accSum then (newLevel, newSum) else (accLevel, accSum)) 
                                 (listLevelSums tree) 

getLevel :: (Num a) => BTree a -> Int -> [a]
getLevel Nil _ = []
getLevel (Node value left right) 1 = [value] 
getLevel (Node value left right) level = getLevel left (level - 1) ++ getLevel right (level - 1)

listLevelSums :: (Num a) =>  BTree a -> [(Int , a)]
listLevelSums Nil = []
listLevelSums tree = zip [1..] $ map (\ x -> sum x) $ takeWhile (not.null) $ map (getLevel tree) [1..]

tree1 :: BTree Int
tree1 = Node 94 (Node 73 Nil (Node 80 Nil Nil)) (Node 87 (Node 73 Nil Nil) Nil)

tree2 :: BTree Int
tree2 = Node 94 (Node 73 Nil (Node 80 Nil Nil)) (Node 87 (Node 73 Nil Nil) (Node 7 Nil Nil))

tree3 :: BTree Int
tree3 = Node 94 (Node 0 Nil (Node 42 Nil Nil)) (Node 0 (Node 42 Nil Nil) (Node 42 Nil Nil))

niltree :: BTree Int
niltree = Nil

data BTree a = Nil | Node a (BTree a) (BTree a)