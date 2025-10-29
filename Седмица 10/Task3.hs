main::IO()
main = do
    print $ perimeter (Circle 5) == 31.41592653589793
    print $ perimeter (Rectangle 2.5 4.5) == 14
    print $ perimeter (Rectangle 5.5 20.6) == 52.2
    print $ perimeter (Triangle 5.3 3.9 4.89) == 14.09
    print $ perimeter (Cylinder 2.5 10) == 30
    print $ perimeter (Rectangle 2 7) == 18
    --my test

    print $ area (Circle 5) == 78.53981633974483
    print $ area (Rectangle 2.5 4.5) == 11.25
    print $ area (Rectangle 5.5 20.6) == 113.30000000000001
    print $ area (Triangle 5.3 3.9 4.89) == 9.127927385194024
    print $ area (Cylinder 20 30) == 6283.185307179587
    print $ area (Triangle 3.0 4.0 5.0) == 6
    --my test

perimeter :: (Floating a) => Shape a -> a
perimeter (Circle r) = pi * 2 * r
perimeter (Rectangle l w) = 2 * (l + w)
perimeter (Triangle a b c) = a + b + c
perimeter (Cylinder r h) = 4 * r + 2 * h 

area :: (Floating a) => Shape a -> a
area (Circle r) = pi * r ^ 2
area (Rectangle l w) = l * w
area (Triangle a b c) = let p = (a + b + c)/2
                            in sqrt (p* (p-a) * (p-b) * (p-c))
area (Cylinder r h) = 2* pi * r * r + 2 * pi * r * h  

data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
 deriving (Eq , Ord , Show, Read)
