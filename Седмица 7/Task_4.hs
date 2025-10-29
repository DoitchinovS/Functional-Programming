main :: IO()
main = do
    print $ areAmicable 200 300 == False
    print $ areAmicable 220 284 == True
    print $ areAmicable 284 220 == True
    print $ areAmicable 1184 1210 == True
    print $ areAmicable 2620 2924 == True
    print $ areAmicable 6232 6368 == True
    print $ areAmicable 123 0 == False
    --my test

areAmicable :: Int -> Int -> Bool
areAmicable a b = divSum a == b && divSum b == a

divSum :: Int -> Int
divSum n = sum [x | x <- [1 .. n-1], mod n x == 0]