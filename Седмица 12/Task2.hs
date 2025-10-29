main :: IO()
main = do
    print $ highestCapital [(Country "Bulgaria" "Sofia" [(City "Varna" 0 16), (City "Plovdiv" 120 14), (City "Sofia" 420 13)]), (Country "Germany" "Berlin" [(City "Munchen" 200 15), (City "Berlin" 150 12), (City "Ulm" 210 15)]), (Country "France" "Paris" [(City "Paris" 180 15), (City "Nice" 0 14), (City "Lyon" 500 13)])] == "Bulgaria"
  --  print $ highestCapital [] -- throws an error
  --my test
    print $ highestCapital [(Country "Spain" "Madrid" [(City "Malaga" 50 24), (City "Bilbao" 0 27), (City "Madrid" 100 28)]), (Country "Sweden" "Stockholm" [(City "Malmo" 120 5), (City "Stockholm" 230 2)])] == "Sweden"
    --my test

highestCapital :: [Country] -> Name
highestCapital [] = error "Empty list!"
highestCapital countries = getCountryName $ foldl1 (\ acc countr -> if getCapitalElev acc > getCapitalElev countr then acc else countr) countries
 where
    getCountryName :: Country -> Name
    getCountryName (Country name _ _ ) = name 

getCapitalElev :: Country -> Int
getCapitalElev (Country name capital cities) = getEval $ [(City name elev temp) | (City name elev temp) <- cities, name == capital]
 where
    getEval :: [City] -> Int
    getEval [] = error "There is no capital"
    getEval ((City name elev temp):cities) = elev

   

type Name = String
type Capital = Name
type AvgYearlyTemperature = Double
type Elevation = Int

data City = City Name Elevation AvgYearlyTemperature
data Country = Country Name Capital [City]

