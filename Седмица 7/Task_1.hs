
main::IO()
main=do

-- Tests
    print $ sqAvg 5 0 == 12.5
    print $ sqAvg 10 13 == 134.5
    print $ sqAvg (-3) (-2) == 6.5
-- my test

sqAvg :: Int -> Int -> Double
sqAvg a b = (/ 2) $ fromIntegral $ a*a + b*b


   