import Data.List
main:: IO()
main = do
    print $ sumUnique [[1,2,3,2],[1,-4],[1]] == 2
    print $ sumUnique [[1,2,3,2],[-4,-4],[5]] == 9 -- (= 1 + 3 + 5)
    print $ sumUnique [[2,2,2],[3,3,3],[4,4,4]] == 0
    print $ sumUnique [[1,2,3],[4,5,6],[7,8,9]] == 45
    print $ sumUnique [[2,3,4,5,2,4], [1, 3, 3, 4], [1]] == 14
    --my test

sumUnique :: (Ord a, Num a) => [[a]] -> a
sumUnique = sum . map (sum . concat . filter ((==1) . length) . group . sort)