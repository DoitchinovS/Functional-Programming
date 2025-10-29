import Data.Char
main::IO()
main = do
    print $ sumSpecialPrimes 5 2 == 392
    print $ sumSpecialPrimes 5 3 == 107
    print $ sumSpecialPrimes 10 3 == 462
    print $ sumSpecialPrimes 6 7 == 246
    --my test
    

sumSpecialPrimes :: Int -> Int -> Int
sumSpecialPrimes n d = sum $ take n [x | x <- [1..], isPrime x, containDigit x d]

isPrime :: Int -> Bool
isPrime n = 1 == sum [x | x <- [1 .. n-1], mod n x == 0]

containDigit :: Int -> Int -> Bool
containDigit n d = elem d $ map digitToInt $ show n
