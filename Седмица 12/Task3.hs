main :: IO ()
main = do
  print $ maxDepthBlueNode colorTree == 2
  print $ maxDepthBlueNode secondColorTree == 3
  --my test
   
  --print $ maxDepthBlueNode nilColorTree  --error "No blue nodes in an empty tree"
  --my test
  
maxDepthBlueNode :: Tree -> Int
maxDepthBlueNode Empty = error "No blue nodes in an empty tree"
maxDepthBlueNode t
 | null (helper t 0) = error "No blue nodes in the tree"
 | otherwise = maximum $ helper t 0
  where
    helper :: Tree -> Int -> [Int]
    helper Empty _ = []
    helper (Node color left right) depth
     | color == Blue = helper left (depth + 1) ++ [depth] ++ helper right (depth + 1)
     | otherwise = helper left (depth + 1) ++ helper right (depth + 1)
    

colorTree :: Tree
colorTree = Node Blue (Node Red (Node Green Empty Empty) Empty) (Node Red (Node Blue (Node Green Empty Empty) (Node Red Empty Empty)) Empty)

secondColorTree :: Tree
secondColorTree = Node Blue (Node Red (Node Green Empty Empty) (Node Blue (Node Blue Empty Empty) Empty)) (Node Blue (Node Red Empty Empty) Empty)

nilColorTree :: Tree
nilColorTree = Empty

data Color = Red | Green | Blue
 deriving (Eq)
data Tree = Empty | Node Color Tree Tree