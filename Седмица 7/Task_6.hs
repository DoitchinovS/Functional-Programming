main :: IO()
main = do
    print $ isPrimeG 1 == False
    print $ isPrimeG 2 == True
    print $ isPrimeG 3 == True
    print $ isPrimeG 6 == False
    print $ isPrimeG 61 == True
    print $ isPrimeG 37 == True
    --my test

    print $ isPrimeLC 1 == False
    print $ isPrimeLC 2 == True
    print $ isPrimeLC 3 == True
    print $ isPrimeLC 6 == False
    print $ isPrimeLC 61 == True
    print $ isPrimeLC 29 ==True
    --my test


isPrimeG :: Int -> Bool
isPrimeG n = helper n 2
 where
    helper :: Int -> Int -> Bool
    helper n divisor
     | n==1 = False
     | divisor >= n = True
     | mod n divisor == 0 = False
     | otherwise = helper n (divisor + 1)

isPrimeLC :: Int -> Bool
isPrimeLC n = 1 == sum [x | x <- [1 .. n-1], mod n x == 0]
    