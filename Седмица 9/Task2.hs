main :: IO()
main = do
     print $ removeFirst 5 [5, 1, 5, 3, 5] == [1, 5, 3, 5]
     print $ removeFirst 3 [5, 1, 5, 3, 5] == [5, 1, 5, 5]
     print $ removeFirst 2 [1, 4, 5] == [1, 4, 5]
     --my test 


removeFirst :: (Eq a) => a -> [a] -> [a]
removeFirst _ [] = []
removeFirst x (y:ys) = helper x (y:ys) []
 where
    helper :: (Eq a) => a -> [a] -> [a] -> [a]
    helper _ [] result = reverse result
    helper x (y:ys) result
     | x == y = (reverse result) ++ ys
     | otherwise = helper x ys (y:result)

