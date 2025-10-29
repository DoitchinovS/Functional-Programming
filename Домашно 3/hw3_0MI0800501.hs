import Data.List
main :: IO ()
main = do

    print $ perimeter ["7-F7-",
                       ".FJ|7",
                       "SJLL7",
                       "|F--J",
                       "LJ.LJ"] == 16
    print $ perimeter [ "-.|F7",
                        ".S-7|",
                        "L|7||",
                        "-L-J|",
                        "L|-JF"] == 8
    print $ perimeter  ["F--7",
                        "|F-7",
                        ".S.|",
                        "|L-J"] == 8
    print $ perimeter      [".F--S--7.",
                            ".|F---7|.",
                            ".||...||.",
                            ".|L-7FJ|.",
                            ".L--JL-J."] == 32
    
    ---------------------------------------Tests for task 2 -----------------------------------------------

    print $ numEnclosed    [".F--S--7.",
                            ".|F---7|.",
                            ".||...||.",
                            ".|L-7FJ|.",
                            ".L--JL-J."] == 1

    print $ numEnclosed ["...|.....|...",
                         ".S---7..F--7.",
                         ".|...|--|..|.",
                         ".L---J..L--J.",
                         "...|.....|..."] == 1

    print $ numEnclosed  [".S-------7.",
                          ".|F-----7|.",
                          ".||.....||.",
                          ".|L-7.F-J|.",
                          ".|..|.|..|.",
                          ".L--J.L--J.",
                          "..........."] == 2

    print $ numEnclosed   [".S--------7.",
                           ".|.F----7.|.",
                           ".|.|....|.|.",
                           ".|.|....|.|.",
                           ".|.L-7F-J.|.",
                           ".|...||...|.",
                           ".L---JL---J."] == 3

    print $ numEnclosed ["..F-S-7.",
                         ".FJF-7L7",
                         "FJFJ.L7|",
                         "L-J...LJ"] == 0

-------------------------------------------Task1-------------------------------------------------------

findStart :: [String] -> Coordinate
findStart map = (findRow map , findPosition (map !! findRow map))

findRow :: [String] -> Int
findRow (x:xs)
 | elem 'S' x == True = 0
 | otherwise = 1 + (findRow xs)

findPosition :: String -> Int
findPosition (x:xs)
  | x == 'S' = 0
  | otherwise = 1 + (findPosition xs)

isValidPosition :: [String] -> Coordinate -> Bool
isValidPosition (x:xs) (a,b) = a < length (x:xs) && b < length x && a >= 0 && b >= 0


findSymbol :: [String] -> Coordinate -> Char
findSymbol map (a,b) = (map !! a) !! b  

getNextPosition :: [String] -> Coordinate -> [Coordinate]
getNextPosition map (a,b)
 | findSymbol map (a,b) == '|' = addOne_None (a,b) ++ subOne_None (a,b)
 | findSymbol map (a,b) == '-' = none_addOne (a,b) ++ none_subOne (a,b)
 | findSymbol map (a,b) == 'L' = subOne_None (a,b) ++ none_addOne (a,b)
 | findSymbol map (a,b) == 'J' = subOne_None (a,b) ++ none_subOne (a,b)
 | findSymbol map (a,b) == '7' = addOne_None (a,b) ++ none_subOne (a,b)
 | findSymbol map (a,b) == 'F' = addOne_None (a,b) ++ none_addOne (a,b)
 | findSymbol map (a,b) == 'S' = addOne_None (a,b) ++ subOne_None (a,b) ++ none_addOne(a,b) ++ none_subOne(a,b)
 | otherwise = []
  where
    addOne_None (c,d) = if isValidPosition map (c+1, d) then [(c+1, d)] else []
    subOne_None (c,d) = if isValidPosition map (c-1, d) then [(c-1,d)] else []
    none_addOne (c,d) = if isValidPosition map (c,d+1) then [(c,d+1)] else []
    none_subOne (c,d) = if isValidPosition map (c,d-1) then [(c,d-1)] else []


extendPath :: [String] -> Coordinate -> [Coordinate] -> [Coordinate]
extendPath map initialPos result = nub $ result ++ getNextPosition map initialPos

perimeter :: [String] -> Int
perimeter map = let
                positionAfterS = [x | x <- getNextPosition map (findStart map) ,elem (findStart map) (getNextPosition map x)]
                                                                              
                firstPos = head $ positionAfterS
                finalPos = last $ positionAfterS
                in length $ drawStartCycle map firstPos finalPos [firstPos]
 
drawStartCycle :: [String] -> Coordinate -> Coordinate -> [Coordinate] -> [Coordinate]
drawStartCycle mymap startCoord finalCoord result
 | elem finalCoord result = findStart mymap : result
 | otherwise = drawStartCycle mymap startCoord finalCoord (delete (findStart mymap) (extendPath mymap (last result) result))

---------------------------------------------------Task 2 ---------------------------------------------------------------

listAllIslands :: [String] -> [[Coordinate]]
listAllIslands mymap = delete [] $ nub $ map (\ x -> listWholeIsland mymap x []) (listAllCoords mymap)
 where
  listAllCoords :: [String] -> [Coordinate]
  listAllCoords (x:xs) = [(a,b) | a <- [0.. length (x:xs)], b <- [0.. length x]]

listWholeIsland :: [String] -> Coordinate -> [Coordinate] -> [Coordinate]
listWholeIsland map (a,b) result = nub $ sortIsland $ helper map (a,b) result
 where
  helper :: [String] -> Coordinate -> [Coordinate] -> [Coordinate]
  helper map (a,b) result
   | not (canIncludePosition map (a,b) result) = result
   | not (canIncludePosition map (a+1,b) result || canIncludePosition map (a-1,b) result 
          || canIncludePosition map (a,b+1) result || canIncludePosition map (a,b-1) result) = (a,b):result 
   | otherwise = helper map (a+1,b) ((a,b):result) ++ helper map (a-1,b) ((a,b):result)
                ++ helper map (a,b+1) ((a,b):result) ++ helper map (a,b-1) ((a,b):result)
   
  canIncludePosition :: [String] -> Coordinate -> [Coordinate] -> Bool
  canIncludePosition map currentCoord result = isValidPosition map currentCoord && not (elem currentCoord result) 
                                              && findSymbol map currentCoord == '.'
  sortIsland :: [Coordinate] -> [Coordinate]
  sortIsland = sortOn snd . sortOn fst


isFromFrame :: [String] -> Coordinate -> Bool
isFromFrame (x:xs) (a,b) = (a == 0 && b < length x) || (a == length xs && b < length x)
                         || (b == 0 && a < length (x:xs)) || (b == length x - 1 && a < length (x:xs))


isOuterPoint :: [String] -> Coordinate -> [Coordinate] -> Bool
isOuterPoint map (a,b) visited
 | not (null visited) && elem (a,b) visited = False
 | not (isValidPosition map (a,b)) = False
 | elem (a,b) (drawStartCycle map firstPos finalPos [firstPos]) = False
 | isFromFrame map (a,b) = True
 | otherwise = isOuterPoint map (a+1,b) ((a,b):visited) || isOuterPoint map (a-1,b) ((a,b):visited) 
                || isOuterPoint map (a,b+1) ((a,b):visited) || isOuterPoint map (a,b-1) ((a,b):visited) 
  where
    positionAfterS = [x | x <- getNextPosition map (findStart map), elem (findStart map) (getNextPosition map x)]
    firstPos = head $ positionAfterS
    finalPos = last $ positionAfterS

numEnclosed :: [String] -> Int
numEnclosed map = length $ [island | island <- listAllIslands map , not $ isOuterIsland map island]

isOuterIsland :: [String] -> [Coordinate] -> Bool
isOuterIsland map island = all (\ x -> isOuterPoint map x []) island

type Coordinate = (Int, Int)