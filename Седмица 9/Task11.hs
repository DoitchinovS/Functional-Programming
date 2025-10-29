main :: IO()
main = do
    print $ subLists [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 2 == [[1, 2], [3, 4], [5, 6], [7, 8], [9, 10]]
    print $ subLists [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 4 == [[1, 2, 3, 4], [5, 6, 7, 8], [9, 10]]
    print $ subLists [2.5, 3.6, 4.1, 7.6, 1.9, 2.12, 3.14] 3 == [[2.5, 3.6, 4.1], [7.6, 1.9, 2.12], [3.14]]
    -- my test

subLists :: [a] -> Int -> [[a]]
subLists [] _ = []
subLists xs n =  take n xs : subLists (drop n xs) n



