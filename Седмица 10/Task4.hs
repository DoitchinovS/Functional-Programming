main :: IO()
main = do
    print $ getAreas [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == [78.53981633974483,11.25,113.30000000000001,9.127927385194024,6283.185307179587]
    print $ getAreas [Circle 3, Rectangle 4 5, Rectangle 4 10, Triangle 6 8 10, Cylinder 10 3] == [28.274333882308138, 20.0, 40.0, 24.0 , 816.8140899333462]
   --my test
    print $ maxArea [Circle 5, Rectangle 2.5 4.5, Rectangle 5.5 20.6, Triangle 5.3 3.9 4.89, Cylinder 20 30] == Cylinder 20.0 30.0
    print $ maxArea []
    --my test

getAreas :: (Floating a) => [Shape a] -> [a]
getAreas = map (\ fig -> area fig)

maxArea :: (Floating a, Ord a) => [Shape a] -> Shape a
maxArea [] = error  "No shapes in the list!"
maxArea shapelst = foldl1 (\ sh acc -> if area sh > area acc then sh else acc) shapelst

area :: (Floating a) => Shape a -> a
area (Circle r) = pi * r ^ 2
area (Rectangle l w) = l * w
area (Triangle a b c) = let p = (a + b + c)/2
                            in sqrt (p* (p-a) * (p-b) * (p-c))
area (Cylinder r h) = 2* pi * r * r + 2 * pi * r * h  


data Shape a = Circle a | Rectangle a a | Triangle a a a | Cylinder a a
 deriving (Eq, Ord, Show, Read)

