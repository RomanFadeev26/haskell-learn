data AuthorName = AuthorName {
    firstName :: String
    , lastName :: String
}

data Spoiler = Spoiler String

data Car = Car String Int

data SportsCar = SportsCar Car Spoiler

type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName
            | NameWithMiddle FirstName MiddleName LastName 
            | NameWithInitials Char Char LastName
            | FirstNameWithTwoInitials FirstName Char Char

data Author = Author Name
data Artist = Person Name | Band String

data Creator = AuthorCreator Author | ArtistCreator Artist

hpLovecraft :: Creator
hpLovecraft = AuthorCreator (Author ( NameWithInitials 'H' 'P' "Lovecraft"))

data Book = Book {
    author :: Creator
    , isbn :: String
    , bookTitle :: String
    , bookYear :: Int
    , bookPrice :: Double
}

data VinylRecord = VinylRecord {
    artist :: Creator
    , recordTitle :: String
    , recordYear :: Int
    , recordPrice :: Double
}

data CollectibleToy = CollectibleToy {
    name :: String
    , description :: String
    , toyPrice :: Double
}

data Pamphlet = Pamphlet {
    pamphletTitle :: String
    , pamphletDescription :: String
    , pamphletContact :: String
}

data StoreItem = BookItem Book | RecordItem VinylRecord | ToyItem CollectibleToy | PamphletItem Pamphlet

price :: StoreItem -> Double
price (BookItem book) = bookPrice book
price (RecordItem record) = recordPrice record
price (ToyItem toy) = toyPrice toy
price (PamphletItem _) = 0.0

-- madeBy :: StoreItem -> String
-- madeBy (BookItem book) = show (author book)
-- madeBy (RecordItem record) = show (artist record)
-- madeBy _ = "unknown"

data Circle = Circle {
    radius :: Double
}

data Square = Square {
    side :: Double
}

data Rectangle = Rectangle {
    width :: Double
    , height :: Double
}

data Shape = ShapeCircle Circle | ShapeSquare Square | ShapeRectangle Rectangle

perimeter :: Shape -> Double
perimeter (ShapeCircle a) = ((2 * pi) * (radius a))
perimeter (ShapeSquare a) = 4 * (side a)
perimeter (ShapeRectangle a) = 2 * ((width a) + (height a))

area :: Shape -> Double
area (ShapeCircle a) = pi * ((radius a) ^ 2)
area (ShapeSquare a) = (side a) ^ 2
area (ShapeRectangle a) = (width a) * (height a)