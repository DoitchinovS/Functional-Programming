main:: IO()
main = do
    print $ primesInRange 1 100 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
    print $ primesInRange 100 1 == [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97]
    print $ primesInRange 100 110 == [101, 103, 107, 109]
    --my test

isPrime :: Int -> Bool
isPrime n = 1 == sum [x | x <- [1 .. n-1], mod n x == 0]

primesInRange :: Int -> Int -> [Int]
primesInRange low up = [x | x <- [min low up .. max low up], isPrime x]