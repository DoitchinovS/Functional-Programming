main :: IO()
main = do
    print $ isPalindrome 6 == True
    print $ isPalindrome 1010 == False
    print $ isPalindrome 505 == True
    print $ isPalindrome 123321 == True
    print $ isPalindrome 654 == False
    print $ isPalindrome 0 == True
    --my test
    
    


isPalindrome :: Int -> Bool
isPalindrome n = n == rev n
 
rev :: Int -> Int
rev n = read $ reverse $ show n



