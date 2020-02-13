import qualified Data.Map as Map
import Data.List

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organsPairs :: [(Int, Organ)]
organsPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organsPairs

possibleDrawers :: [Int]
possibleDrawers = [1..50]

getDrawerContents :: [Int] -> Map.Map Int Organ -> [Maybe Organ]
getDrawerContents ids catalog = map getContents ids
                                where getContents = (\id -> Map.lookup id catalog)

countEmptyDrawers :: Int -> Maybe Organ -> Int
countEmptyDrawers number (Just _) = number
countEmptyDrawers number Nothing = number + 1

emptyDrawers :: Int
emptyDrawers = foldl countEmptyDrawers 0 (getDrawerContents possibleDrawers organCatalog)

availableOrgans :: [Maybe Organ]
availableOrgans = getDrawerContents possibleDrawers organCatalog

countOrgan :: Organ -> [Maybe Organ] -> Int
countOrgan organ catalog = (length . (filter getOrgans)) catalog
                            where getOrgans = (\drawerContent -> drawerContent == Just organ)

isSomething :: Maybe Organ -> Bool
isSomething Nothing = False
isSomething (Just _) = True

justTheOrgans :: [Maybe Organ]
justTheOrgans = filter isSomething availableOrgans

showOrgan :: Maybe Organ -> String
showOrgan (Just organ) = show organ
showOrgan Nothing = ""

organList :: [String]
organList = map showOrgan justTheOrgans

cleanList :: String
cleanList = intercalate ", " organList

numOrZero :: Maybe Int -> Int
numOrZero (Just a) = a
numOrZero Nothing = 0

data Container = Vat Organ | Cooler Organ | Bag Organ

instance Show Container where
    show (Vat organ) = show organ ++ " in a vat"
    show (Cooler organ) = show organ ++ " in a cooler"
    show (Bag organ) = show organ ++ " in a bag"

data Location = Lab | Kitchen | Bathroom deriving Show

organToContainer :: Organ -> Container
organToContainer Brain = (Vat Brain)
organToContainer Heart = (Cooler Heart)
organToContainer organ = (Bag organ)

placeInLocation :: Container -> (Location, Container)
placeInLocation (Vat a) = (Lab, Vat a)
placeInLocation (Cooler a) = (Lab, Cooler a)
placeInLocation (Bag a) = (Kitchen, Bag a)

-- process :: Organ -> (Location, Container)
-- process = placeInLocation . organToContainer

-- report :: Maybe (Location, Container) -> String
-- report (Just (location, container)) = show container ++ " in the " ++ show location
-- report Nothing = "container not found"

-- processAndReport :: Maybe Organ -> String
-- processAndReport (Just organ) = report (process organ)
-- processAndReport Nothing = "error, id not found"

-- processRequest :: Int -> Map.Map Int Organ -> String
-- processRequest id drawers = processAndReport drawer
--                             where drawer = Map.lookup id drawers

apMaybe :: (a -> b) -> Maybe a -> Maybe b
apMaybe func (Just a) = Just (func a)
apMaybe Nothing = Nothing

maybeMap :: [Maybe a] -> [Maybe b]
maybeMap func = map (apMaybe func)