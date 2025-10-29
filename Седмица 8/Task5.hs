main:: IO()
main = do
    print $ reverseOrdSuff 37563 == 36
    print $ reverseOrdSuff 32763 == 367
    print $ reverseOrdSuff 32567 == 7
    print $ reverseOrdSuff 32666 == 6
    print $ reverseOrdSuff 1678542 == 2458

reverseOrdSuff :: Int -> Int
reverseOrdSuff n = helper n 0
 where
    helper :: Int -> Int -> Int
    helper leftover result
        | mod leftover 10 >= mod (div leftover 10) 10 = 10*result + mod leftover 10
        | otherwise = helper (div leftover 10) (10* result + mod leftover 10) 