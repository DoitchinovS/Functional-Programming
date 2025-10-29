main :: IO ()
main = do
 print $ minDepthGreenNode colorTree == 2
 print $ minDepthGreenNode secondColorTree == 3
 -- print $ minDepthGreenNode emptyTree error Empty tree 
 --my test





minDepthGreenNode :: Tree -> Int
minDepthGreenNode Empty = error "No green nodes in an empty tree!"
minDepthGreenNode t
 | null (helper t 0) = error "There are no green nodes!"
 | otherwise = minimum $ helper t 0
 where
    helper :: Tree -> Int -> [Int]
    helper Empty _ = []
    helper (Node color left right) depth 
     | color == Green = [depth] ++ helper left (depth + 1) ++ helper right (depth + 1)
     | otherwise = helper left (depth + 1) ++ helper right (depth + 1)



colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)

secondColorTree :: Tree
secondColorTree = Node Blue (Node Blue (Node Red Empty Empty) (Node Blue Empty Empty)) (Node Red Empty (Node Blue (Node Green Empty Empty) Empty))

emptyTree :: Tree
emptyTree = Empty

data Color = Red | Green | Blue
 deriving (Eq, Enum)
data Tree = Empty | Node Color Tree Tree