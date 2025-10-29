main :: IO ()
main = do
  print $ controller "" == ""
  print $ controller ".........." == "0000000000"
  print $ controller "P...." == "12345"
  print $ controller "P.P.." == "12222"
  print $ controller "..P...O..." == "0012343210"
  print $ controller "P......P......" == "12345554321000"
  print $ controller "P......P......" == "12345554321000"
  print $ controller "P.P.." == "12222"
  print $ controller "P.P.P...."  == "122234555"
  print $ controller ".....P.P........P...." == "000001222222222234555"
  print $ controller ".....P......P.P..P...."  == "0000012345554333321000"
  print $ controller "P.O...." == "1210000"
  print $ controller "P......P.O...." == "12345554345555"
  print $ controller "P..OP..P.." == "1232222100"
  print $ controller "P......P..OP..P..." == "123455543233334555"
  print $ controller "..P...O....." == "001234321000"

controller :: String -> String
controller "" = ""
controller (x:xs) = helper xs "" (changeState Standing_Opening x 0) 0
   where
      helper :: String -> String -> DoorState -> Int -> String
      helper [] result Opening 5 = result ++ show 5
      helper [] result Opening levelOpen = result ++ show(levelOpen + 1)

      helper [] result Standing_Closing levelOpen = result ++ show levelOpen
      helper [] result Standing_Opening levelOpen = result ++ show levelOpen

      helper [] result Closing 0 = result ++ show 0
      helper [] result Closing levelOpen = result ++ show (levelOpen - 1)
      
      helper (x:xs) result state levelOpen
       | state == Opening = helper xs (result ++ show (levelOpen + 1)) (changeState state x (levelOpen + 1)) (levelOpen + 1)
       | state == Closing = helper xs (result ++ show (levelOpen - 1)) (changeState state x (levelOpen - 1)) (levelOpen - 1)
       | otherwise = helper xs (result ++ (show levelOpen)) (changeState state x levelOpen) levelOpen
       

changeState :: DoorState -> Char -> Int -> DoorState
changeState current action levelOpened
 | (current == Closing && levelOpened == 0) = Standing_Opening
 | (current == Opening && levelOpened == 5) = Standing_Closing
 --fully closed or opened
 | current == Standing_Opening && action == 'P' = Opening
 | current == Standing_Closing && action == 'P'= Closing
 | current == Opening && action == 'P' = Standing_Opening
 | current == Closing && action == 'P' = Standing_Closing 
 --posibilities for P
 | (current == Opening && action == 'O' && levelOpened < 5) = Closing
 | (current == Closing && action == 'O' && levelOpened > 0) = Opening
 --posibilities for O
 | otherwise = current
    
data DoorState = Closing | Opening | Standing_Closing | Standing_Opening
 deriving (Eq)