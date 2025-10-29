import Data.List
main :: IO()
main = do
    print $ isValidSudoku [[1, 2, 3, 4, 5, 6, 7, 8],
                    [5, 6, 7, 8, 1, 2, 3, 4],
                    [2, 3, 4, 1, 6, 5, 8, 7],
                    [6, 5, 8, 7, 2, 3, 4, 1],
                    [3, 4, 1, 2, 7, 8, 5, 6],
                    [7, 8, 5, 6, 3, 4, 1, 2],
                    [4, 1, 2, 3, 8, 7, 6, 5],
                    [8, 7, 6, 5, 4, 1, 2, 3]] == True

    print $ isValidSudoku  [[1, 6, 3, 4, 5, 6, 7, 8],
                    [5, 2, 7, 8, 1, 2, 3, 4],
                    [2, 3, 4, 1, 6, 5, 8, 7],
                    [6, 5, 8, 7, 2, 3, 4, 1],
                    [3, 4, 1, 2, 7, 8, 5, 6],
                    [7, 8, 5, 6, 3, 4, 1, 2],
                    [4, 1, 2, 3, 8, 7, 6, 5],
                    [8, 7, 6, 5, 4, 1, 2, 3]] == False

    print $ isValidSudoku [[1, 2, 3, 4, 5, 6, 7, 8],
                   [5, 6, 7, 8, 1, 2, 3, 4],
                   [2, 3, 4, 1, 6, 5, 8, 7],
                   [6, 5, 8, 7, 2, 3, 4, 1],
                   [3, 4, 1, 2, 2, 8, 5, 6],
                   [7, 8, 5, 6, 3, 4, 1, 2],
                   [4, 1, 2, 3, 8, 7, 6, 5],
                   [8, 7, 6, 5, 4, 1, 2, 3]] == False


isValidSudoku :: [[Int]] -> Bool
isValidSudoku xss = isSizeCorrect xss && areNumbersCorrectInterval xss 
                    && areRowNumsUnique xss && areColNumsUnique xss && areBoxNumsUnique xss

areRowNumsUnique :: [[Int]] -> Bool
areRowNumsUnique [] = True
areRowNumsUnique (xs:xss) = length (nub xs) == 8 && (areRowNumsUnique xss)   

areColNumsUnique :: [[Int]] -> Bool
areColNumsUnique xss = areRowNumsUnique (transpose xss)

areBoxNumsUnique :: [[Int]] -> Bool
areBoxNumsUnique [] =True
areBoxNumsUnique (xs:ys:xss) = length (nub ((take 4 xs) ++ (take 4 ys))) == 8 &&
                               length (nub ((drop 4 xs) ++ (drop 4 ys))) == 8 && areBoxNumsUnique xss

isSizeCorrect :: [[Int]] -> Bool
isSizeCorrect [] = False
isSizeCorrect (xs:xss) = length (xs:xss) == 8 && length xs == 8

areNumbersCorrectInterval :: [[Int]] -> Bool
areNumbersCorrectInterval [] = True
areNumbersCorrectInterval (xs:xss) =  (correctIntervalRow xs) && (areNumbersCorrectInterval xss)
 where
    correctIntervalRow :: [Int] -> Bool
    correctIntervalRow [] = True
    correctIntervalRow (x:xs) = 1 <= x && x <= 8 && (correctIntervalRow xs)
