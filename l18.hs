import Data.Char

data Box a = Box a deriving Show

wrap :: a -> Box a
wrap a = Box a

unwrap :: Box a -> a
unwrap (Box a) = a

data Triple a = Triple a a a deriving Show

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

