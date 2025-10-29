main :: IO()
main = do
    print $ isPerfect 1 == False
    print $ isPerfect 6 == True
    print $ isPerfect 495 == False
    print $ isPerfect 33550336 == True
    print $ isPerfect 200 == False
    --my test

isPerfect :: Int -> Bool
isPerfect n = n == sum [x | x <- [1 .. n-1], mod n x == 0]