import Data.Char
import qualified Data.Map as Map

data Box a = Box a deriving Show

wrap :: a -> Box a
wrap a = Box a

unwrap :: Box a -> a
unwrap (Box a) = a

boxMap :: (a -> b) -> Box a -> Box b
boxMap func (Box a) = Box (func a)

data Triple a = Triple a a a deriving Show

tripleMap :: (a -> b) -> Triple a -> Triple b
tripleMap func (Triple a b c) = Triple (func a) (func b) (func c)

type Point3D = Triple Double

aPoint :: Point3D
aPoint = Triple 0.5 1.6 1.7

type FullName = Triple String

hLovecraft :: FullName
hLovecraft = Triple "Howard" "Filip" "Lovecraft"

type Initials = Triple Char

initials :: Initials
initials = Triple 'H' 'F' 'L'

first :: Triple a -> a
first (Triple a _ _) = a

second :: Triple a -> a
second (Triple _ a _) = a

third :: Triple a -> a
third (Triple _ _ a) = a

tripleToList :: Triple a -> [a]
tripleToList (Triple a b c) = [a,b,c]

transform :: (a -> a) -> Triple a -> Triple a
transform f (Triple a b c) = Triple (f a) (f b) (f c)

data List a = Empty | Cons a (List a) deriving Show

ourListEx :: List Char
ourListEx = Cons 'c' (Cons 'a' (Cons 't' Empty))

ourListExInt :: List Int
ourListExInt = Cons 1 (Cons 2 (Cons 3 Empty))

ourMap :: (a -> b) -> List a -> List b
ourMap _ Empty = Empty
ourMap func (Cons a rest) = Cons (func a) (ourMap func rest)

type Item = (String, Int)

itemCount1 :: Item
itemCount1 = ("Eraser", 1)

itemCount2 :: Item
itemCount2 = ("Pencils", 2)

itemCount3 :: Item
itemCount3 = ("Pens", 3)

itemInventory :: [Item]
itemInventory = [itemCount1, itemCount2, itemCount3]

data Organ = Heart | Brain | Kidney | Spleen deriving (Show, Eq, Ord, Enum)

organs :: [Organ]
organs = [Heart, Heart, Brain, Spleen, Spleen, Kidney]

ids :: [Int]
ids = [2,7,13,14,21,24]

organsPairs :: [(Int, Organ)]
organsPairs = zip ids organs

organCatalog :: Map.Map Int Organ
organCatalog = Map.fromList organsPairs

values :: [Organ]
values = map snd (Map.toList organCatalog)

allOrgans :: [Organ]
allOrgans = [Heart .. Spleen]

organCounts :: [Int]
organCounts = map countOrgans allOrgans
                where countOrgans = (\organ -> (length (filter (== organ)  organs)))

organsInventory :: Map.Map Organ Int
organsInventory  = Map.fromList (zip allOrgans organCounts)