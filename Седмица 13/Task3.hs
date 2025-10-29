import Data.List
main :: IO ()
main = do
    print $ levelSum numberBTree 1 == 11 -- (5 + 6)
    print $ levelSum numberBTree 2 == 25
    --my test
    print $ cone numberBTree == True
    print $ cone secondNumberTree == True
    --my test



levelSum :: Btree -> Int -> Int
levelSum Nil _ = 0
levelSum t k = sum $ getLevel t k

cone :: Btree -> Bool
cone Nil = True
cone t = (sort $ getLevelSums t) == getLevelSums t 

getLevel :: Btree -> Int -> [Int]
getLevel Nil _ = []
getLevel (Node value left right) 0 = [value]
getLevel (Node value left right) k = getLevel left (k-1) ++ getLevel right (k-1)

getLevelSums :: Btree -> [Int]
getLevelSums Nil = []
getLevelSums t = map sum $ takeWhile (not.null) $ map (getLevel t) [0..]

secondNumberTree :: Btree
secondNumberTree = Node 7 (Node 8 (Node 10 Nil Nil) (Node 11 Nil Nil)) (Node 9 (Node 12 Nil Nil) (Node 13 Nil Nil))

numberBTree :: Btree
numberBTree = Node 10 (Node 5 (Node 1 Nil Nil) (Node 9 Nil Nil)) (Node 6 (Node 8 Nil Nil) (Node 7 Nil Nil))

data Btree = Nil | Node Int Btree Btree
 deriving (Eq, Show)

