main :: IO ()
main = do
    print $ closestAverage [(Temp 1 23.6), (Temp 6 24.2), (Temp 11 24.2), (Temp 16 21.2), (Temp 21 23.8), (Temp 26 26.5), (Temp 31 24.5)] == 21
    print $ closestAverage [(Temp 1 27.8), (Temp 4 22.4), (Temp 6 25.6), (Temp 3 21.2), (Temp 8 29.1), (Temp 2 24.5)] == 6
    --my test

closestAverage :: [Measuring] -> Int
closestAverage [] = error "There are no measurings!"
closestAverage measurings = day
 where
    averageValue = calculateAverage measurings
    (Temp day measuring) = foldl1 (\ (Temp accDay accMeas) (Temp nextDay nextMeas) -> 
                                                  if (abs (averageValue - accMeas)) < (abs (averageValue - nextMeas)) 
                                                    then (Temp accDay accMeas) else (Temp nextDay nextMeas)) measurings
    
 

calculateAverage :: [Measuring] -> Float
calculateAverage measurings = (tempretureSum measurings) / (fromIntegral $ length measurings)
 where
    tempretureSum :: [Measuring] -> Float
    tempretureSum measurings = sum $ [ tempr | (Temp day tempr) <- measurings]

data Measuring = Temp Int Float