import qualified Data.Map as Map

type LatLong = (Double, Double)

locationDB :: Map.Map String LatLong
locationDB = Map.fromList [("Arkham", (42.6054,-70.7829))
                          , ("Innsmouth", (42.8250,-70.8150))
                          , ("Carsoca", (29.9714,-90.7694))
                          , ("New York", (40.7776,-73.9691))]

toRadians :: Double -> Double
toRadians degrees = degrees * pi / 180

latLongToRads :: LatLong -> (Double, Double)
latLongToRads (lat, long) = (rlat, rlong)
    where rlat = toRadians lat
          rlong = toRadians long

haversine :: LatLong -> LatLong -> Double
haversine coords1 coords2 = earthRadius  * c
    where (rlat1, rlong1) = latLongToRads coords1
          (rlat2, rlong2) = latLongToRads coords2
          dlat = rlat2 - rlat1
          dlong = rlong2 - rlong1
          a = sin (dlat/2) ^ 2 + cos rlat1 * cos rlat2 * sin (dlong/2)^2
          c = 2 * atan2 (sqrt a) (sqrt (1-a))
          earthRadius = 3961.0

printDistance :: Maybe Double -> IO ()
printDistance Nothing = putStrLn "Error, invalid city entered"
printDistance (Just dist) = putStrLn (show dist ++ " miles")

addMaybe :: Maybe Int -> Maybe Int -> Maybe Int
addMaybe Nothing _ = Nothing
addMaybe _ Nothing  = Nothing
addMaybe (Just a) (Just b) = Just (a + b)

main :: IO()
main = do
    putStrLn "Enter the starting city name: "
    city1 <- getLine
    let startingCity = Map.lookup city1 locationDB
    putStrLn "Enter destination city: "
    city2 <- getLine
    let destinationCity = Map.lookup city2 locationDB
    printDistance (haversine <$> destinationCity <*> startingCity)


-- haversineIO :: IO LatLong -> IO LatLong -> IO Double
-- haversineIO ll1 ll2 = do
--     latLong1 <- ll1
--     latLong2 <- ll2
--     return (haversine latLong2 latLong1)

haversineIO :: IO LatLong -> IO LatLong -> IO Double
haversineIO ll1 ll2 = haversine <$> ll2 <*> ll1