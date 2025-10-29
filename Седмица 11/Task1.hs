main :: IO()
main = do
    print $ height numberBTree == 4
    print $ height charBTree == 3
    print $ height doubleBTree == 3
    print $ height nilTree == 0
    --my test

    print $ average numberBTree == 16.22
   -- print $ average nilTree -- error Empty Tree
    --my test
   -- print $ average charBTree -- should not work

    print $ sumLeaves numberBTree == 119
    --print $ sumLeaves charBTree -- shouldn't work
    print $ sumLeaves doubleBTree == 17.6
    --my test

    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil Nil))) == False
    print $ areEqual charBTree charBTree == True
    print $ areEqual numberBTree (Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 8 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))) == False
    print $ areEqual nilTree numberBTree == False
    print $ areEqual nilTree nilTree == True
    --my test

    print $ setLevels numberBTree == Node (0,5) (Node (1,12) (Node (2,1) (Node (3,96) Nil Nil) Nil) (Node (2,0) Nil Nil)) (Node (1,4) (Node (2,2) Nil Nil) (Node (2,5) Nil (Node (3,21) Nil Nil)))
    print $ setLevels charBTree == Node (0,'k') (Node (1,'a') (Node (2,'h') Nil Nil) (Node (2,'s') Nil Nil)) (Node (1,'l') (Node (2,'e') Nil Nil) (Node (2,'l') Nil Nil))
    print $ setLevels doubleBTree == Node (0, 2.3) (Node (1, 1.4) (Node (2, 4.7) Nil Nil) (Node (2, 5.8) Nil Nil)) (Node (1, 3.6) Nil (Node (2, 7.1) Nil Nil))
    --my test 

    print $ mirrorTree numberBTree == Node 5 (Node 4 (Node 5 (Node 21 Nil Nil) Nil) (Node 2 Nil Nil)) (Node 12 (Node 0 Nil Nil) (Node 1 Nil (Node 96 Nil Nil)))
    print $ mirrorTree charBTree == Node 'k' (Node 'l' (Node 'l' Nil Nil) (Node 'e' Nil Nil)) (Node 'a' (Node 's' Nil Nil) (Node 'h' Nil Nil))
    print $ mirrorTree doubleBTree == Node 2.3 (Node 3.6 (Node 7.1 Nil Nil) Nil) (Node 1.4 (Node 5.8 Nil Nil) (Node 4.7 Nil Nil))
    --my test


height :: BTree a -> Int
height Nil = 0
height t = maximum $ getLeavesHeight t 0
 where
    getLeavesHeight :: BTree a -> Int -> [Int]
    getLeavesHeight Nil currh = [currh]
    getLeavesHeight (Node value left right) currh = getLeavesHeight right (currh + 1) ++ getLeavesHeight left (currh + 1) 
     
countNodes :: BTree a -> Int
countNodes Nil = 0
countNodes (Node value left right) = 1 + countNodes left + countNodes right

sumNodes :: (Num a) => BTree a -> a
sumNodes Nil = 0
sumNodes (Node value left right) = value + sumNodes left + sumNodes right

average :: BTree Int -> Double
average Nil = error "Empty tree!"
average t = (/ 100).fromIntegral.round.(* 100) $ fromIntegral (sumNodes t) / fromIntegral (countNodes t)

sumLeaves :: (Num a) => BTree a -> a
sumLeaves Nil = 0
sumLeaves (Node value Nil Nil) = value
sumLeaves (Node value left right) = sumLeaves left + sumLeaves right

areEqual :: (Eq a) => BTree a -> BTree a -> Bool
areEqual Nil Nil = True
areEqual Nil _ = False
areEqual _ Nil = False
areEqual (Node value1 left1 right1) (Node value2 left2 right2) = value1 == value2 && (areEqual left1 left2) && (areEqual right1 right2)

setLevels :: BTree a -> BTree (Int, a)
setLevels Nil = error "Empty tree!"
setLevels t = helper t 0
 where
    helper :: BTree a -> Int -> BTree (Int, a)
    helper Nil _ =  Nil
    helper (Node value left right) level = Node (level, value) (helper left (level + 1) ) (helper right (level + 1))  


mirrorTree :: BTree a -> BTree a
mirrorTree Nil = Nil
mirrorTree (Node value left right) = Node value (mirrorTree right) (mirrorTree left)

data BTree a = Nil | Node a (BTree a) (BTree a)
 deriving (Show, Eq)

charBTree :: BTree Char
charBTree = Node 'k' (Node 'a' (Node 'h' Nil Nil) (Node 's' Nil Nil)) (Node 'l' (Node 'e' Nil Nil) (Node 'l' Nil Nil))

numberBTree :: BTree Int
numberBTree = Node 5 (Node 12 (Node 1 (Node 96 Nil Nil) Nil) (Node 0 Nil Nil)) (Node 4 (Node 2 Nil Nil) (Node 5 Nil (Node 21 Nil Nil)))

doubleBTree :: BTree Double
doubleBTree = Node 2.3 (Node 1.4 (Node 4.7 Nil Nil) (Node 5.8 Nil Nil)) (Node 3.6 Nil (Node 7.1 Nil Nil))

nilTree :: BTree Int
nilTree = Nil