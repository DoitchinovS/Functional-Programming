
main :: IO()
main = do
    print $ combine [(1, 2), (1, 2)] == (11, 22)
    print $ combine [(3, 9), (8, 7), (7, 9), (8, 8), (5, 0), (9, 2)] == (377802, 989859)
    print $ combine [(1, 3), (4, 2), (5, 7)] == (125, 347)
    --my test

combine :: [(Int , Int)] -> (Int, Int)
--combine [] = error "No tuples in the list!"
combine  = foldl (\ (accmin , accmax) (x,y) -> (accmin * 10 + min x y, accmax * 10 + max x y)) (0, 0) 

--combineTyped :: (Ord a) => [(a,a)] -> (a, a)
--combineTyped [] = error "No tuples in the list!"
--combineTyped xs = foldl (\  (accmin, accmax) (x,y) -> (accmin ++ show (min x y), accmax ++ show (max x y))) ("", "") xs
