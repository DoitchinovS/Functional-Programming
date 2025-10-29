main :: IO()
main = do
    print $ crossProduct (1, 2, 3) (7, 4, 1) == (-10, 20, -10)
    print $ crossProduct (5, 2, 159) (0, -1, -2) == (155, 10, -5)
    print $ crossProduct (2, 3, 4) (4, 5, 6) == (-2, 4, -2)
    --my test

    print $ magnitude (1, 2, 3) == 3.7416573867739413
    print $ magnitude (7, 4, 1) == 8.12403840463596
    print $ magnitude (-10, 20, -10) == 24.49489742783178
    print $ magnitude (5, 2, 159) == 159.0911688309568
    print $ magnitude (0, -1, -2) == 2.23606797749979
    print $ magnitude (155, 10, -5) == 155.40270267920053
    print $ magnitude (7, 4, 2) == 8.306623862918075
    --my test
    
type Vector a = (a, a, a)

crossProduct :: (Num a) => Vector a -> Vector a -> Vector a
crossProduct (x1, x2, x3) (y1, y2, y3) = (x2 * y3 - x3 * y2, x3 * y1 - x1 * y3, x1 * y2 - x2 * y1)

magnitude :: (Num a, Floating a) => Vector a -> a
magnitude (x, y, z) =  sqrt (x * x + y * y + z * z)