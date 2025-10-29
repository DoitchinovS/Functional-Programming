import Data.List
main :: IO()
main = do
    print $ isImage [1, 2, 3, 4] [2, 3, 4, 5] == (True, 1)
    print $ isImage [1, 2, 3, 4] [4, 5, 6, 7] == (True, 3)
    print $ isImage [4, 5, 6, 7] [1, 2, 3, 4] == (True, -3)
    print $ isImage [1, 2, 3, 4] [4, 5, 6, 6] == (False, 0)
    print $ isImage [1, 2] [-1, -2] == (False, 0)
    print $ isImage [1, 2, 3, 4] [2, 3, 4, 4] == (False, 0)
    print $ isImage [4, 5, 6, 7] [5.5, 6.5, 7.5, 8.5] == (True, 1.5) 
    --my test



findDifference :: (Num a, Eq a) => [a] -> [a] -> [a]
findDifference xs ys = nub $ zipWith (-) ys xs -- the first list is to be subtracted from the second one

isImage :: (Num a, Eq a) => [a] -> [a] -> (Bool , a)
isImage xs ys
 | length (findDifference xs ys) /= 1 = (False, 0)
 | otherwise = (True, (head (findDifference xs ys)))

