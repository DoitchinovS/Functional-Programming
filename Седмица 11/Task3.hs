main :: IO()
main = do
    print $ ordered t1 == True
    print $ ordered t2 == False

    print $ ordered t3 == True
    print $ ordered nilTree == True
    --my tests
    


ordered :: (Ord a) => BTree a -> Bool
ordered Nil = True
ordered (Node interval Nil Nil) = True
ordered (Node interval Nil (Node interRight rightleft rightright)) = isSubInterval interval interRight 
                                                                && ordered (Node interRight rightleft rightright)

ordered (Node interval (Node interLeft leftleft leftright) Nil) = isSubInterval interLeft interval 
                                                                && ordered (Node interLeft leftleft leftright)

ordered (Node interval (Node interLeft leftleft leftright) (Node interRight rightleft rightright)) = isSubInterval interLeft interval
                                                                                                && isSubInterval interval interRight
                                                                                                && ordered (Node interLeft leftleft leftright)
                                                                                                && ordered (Node interRight rightleft rightright)
                                                                                                    
                                                                                            

isSubInterval :: (Ord a) => (a,a) -> (a,a) -> Bool
isSubInterval (a,b) (c,d) = (a >= c) && (b <= d)


data BTree a = Nil | Node (a,a) (BTree a) (BTree a)
 deriving (Show, Eq)

t1 :: BTree Int
t1 = Node (3,10) (Node (5,8) (Node (6,7) Nil Nil) (Node (4,9) Nil Nil)) (Node (2,12) Nil (Node (1,15) Nil Nil))

t2 :: BTree Int
t2 = Node (3,10) (Node (5,8) (Node (6,7) Nil Nil) (Node (7,9) Nil Nil)) (Node (2,12) Nil (Node (1,15) Nil Nil))

t3 :: BTree Int
t3 = Node (7,13) (Node (8,12) (Node (10,11) Nil Nil) Nil) (Node (6,14) Nil (Node (3,15) Nil Nil))

nilTree :: BTree Int
nilTree = Nil