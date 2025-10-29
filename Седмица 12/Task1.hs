main :: IO()
main = do
 print $ myPoly [2.7, 3.0 ..] 2.2 3 == -0.4399999999999998
 print $ myPoly [1, 2 ..] 4 2 == 6
 --my test


myPoly :: (Num a) => [a] -> (a -> Int -> a)
myPoly zs = (\ x y -> product $ take y $ map (\ z -> x-z) zs)


