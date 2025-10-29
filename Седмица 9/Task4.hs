main :: IO()
main = do
    print $ sumRats (2, 5) (5, 2) == (29, 10)
    print $ sumRats (52, 123) (96, 14) == (6268, 861)
    print $ sumRats (2, 5) (3, 5) == (1, 1)
    print $ sumRats (2, 3) (3, 4) == (17, 12)
    --my test
    
    print $ multiplyRats (2, 5) (5, 2) == (1, 1)
    print $ multiplyRats (52, 123) (96, 14) == (832, 287)
    print $ multiplyRats (2, 5) (3, 5) == (6, 25)
    print $ multiplyRats (1, 3) (3, 9) == (1, 9)
    --my test
    --print $ multiplyRats (2, 0) (3, 4) 
    --error 

    print $ divideRats (2, 5) (5, 2) == (4, 25)
    print $ divideRats (52, 123) (96, 14) == (91, 1476)
    print $ divideRats (2, 5) (3, 5) == (2, 3)
    print $ divideRats (4, 5) (6, 5) == (2, 3)
    --my test
    
    print $ areEqual (2, 5) (5, 2) == False
    print $ areEqual (52, 123) (52 * 3, 123 * 3) == True
    print $ areEqual (2, 6) (5, 15) == True
    print $ areEqual (14, 2) (21, 3) == True
    --my test
    
type Rat = (Int, Int)

normalize :: Rat -> Rat
normalize (x, y) = let highestCommonDenom = gcd x y
    in (div x highestCommonDenom, div y highestCommonDenom)


multiplyRats :: Rat -> Rat -> Rat
multiplyRats ( _ , 0) ( _ , _ ) = error "Denominator cannot equal zero!"
multiplyRats ( _ , _) ( _ , 0 ) = error "Denominator cannot equal zero!"
multiplyRats (a, b) (c, d) = normalize $ (a * c, b * d)

areEqual :: Rat -> Rat -> Bool
areEqual ( _ , 0) ( _ , _ ) = error "Denominator cannot equal zero!"
areEqual ( _ , _ ) ( _ , 0) = error "Denominator cannot equal zero!"
areEqual (a, b) (c, d) = fromIntegral a / fromIntegral c == fromIntegral b /fromIntegral d

divideRats :: Rat -> Rat -> Rat
divideRats ( _ , 0) ( _ , _ ) = error "Denominator cannot equal zero!"
divideRats ( _ , _ ) ( _ , 0 ) = error "Denominator cannot equal zero!"
divideRats (a , b) (c , d) = normalize $ (a * d , b * c)

sumRats :: Rat -> Rat -> Rat
sumRats ( _ , 0) ( _ , _ ) = error "Denominator cannot equal zero!"
sumRats ( _ , _ ) ( _ , 0 ) = error "Denominator cannot equal zero!"
sumRats (a, b) (c, d) = normalize $ (a * d + b * c, b * d)