import Data.List(sort)
main :: IO()
main = do
    print $ hasIncDigits 1244 == True
    print $ hasIncDigits 12443 == False
    print $ hasIncDigits 102345 == False
    --my test

hasIncDigits :: Int -> Bool
hasIncDigits n = show n == sort (show n)