import Data.List
import Data.Char

main :: IO()
main = do
    print $ reduceStr "dabAcCaCBAcCcaDD" == "dabCBAcaDD" -- dabAcCaCBAcCcaDD -> dabAaCBAcCcaDD -> dabCBAcCcaDD -> dabCBAcaDD
    print $ reduceStr "daADCcFf" == ""
    --my test
    

reduceStr :: [Char] -> [Char]
reduceStr xs = reverse $ helper xs []
 where
    helper :: [Char] -> [Char] -> [Char]
    helper [] ys = ys
    helper (x:xs) [] = helper xs [x]
    helper (x:xs) (y:ys)
     | isDuplicate x y = helper xs ys
     | otherwise = helper xs (x:y:ys)
 

isDuplicate :: Char -> Char -> Bool
isDuplicate x y = x/=y && (toLower x == y || toUpper x == y)