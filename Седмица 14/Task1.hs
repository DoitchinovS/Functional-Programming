main::IO()
main = do
    print $ prodEvens [1,2,3,4,5,6]  == 15
    print $ prodEvens [7.66, 7, 7.99, 7]  == 61.2034


prodEvens :: (Num a) => [a] -> a
prodEvens = snd.(foldr (\ (newPos, newValue) (accPos, accValue) -> if mod newPos 2 == 0 then (accPos, accValue * newValue) 
                        else (accPos,accValue)) (0,1)).makePairNumPos

makePairNumPos :: (Num a) => [a] -> [(Int, a)]
makePairNumPos [] = []
makePairNumPos xs = zip [0..] xs
