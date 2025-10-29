main :: IO()
main = do
   print $ addOne [1, 2, 3, 4]
   print $ revOneLine 234
   print $ revOneLineMagic 145
   print $ pack [1,9]

addOne :: (Num a) => [a] -> [a]
addOne xs = map (\ x -> x + 1) xs

revOneLine :: Int -> Int
revOneLine n = read $ reverse $ show n 

revOneLineMagic :: Int -> Int
revOneLineMagic = read.reverse.show 

pack :: [Int] -> [[Int]]
pack [] = []
pack (x:xs) = map reverse $ helper [x] xs
 where
    helper :: [Int] -> [Int] -> [[Int]]
    helper res [] = [res]
    helper (r:rs) (y:ys)
     | y == r + 1 = helper (y:r:rs) ys
     | otherwise = (r:rs) : helper [y] ys
