main :: IO()
main = do
--tests
    print $ myGcdG 5 13 == 1
    print $ myGcdG 13 1235 == 13
    print $ myGcdG 24 36 == 12
    --my test

    print $ myGcdPM 5 13 == 1
    print $ myGcdPM 13 1235 == 13
    print $ myGcdPM 81 63 == 9
    --my test

myGcdG :: Int -> Int -> Int
myGcdG a b
 | (b == 0) = a 
 | otherwise = myGcdG b  (mod a b)

myGcdPM :: Int -> Int -> Int
myGcdPM a 0 = a
myGcdPM a b = myGcdPM b (mod a b)
