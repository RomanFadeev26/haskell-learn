-- data SixSidedDice = S1 | S2 | S3 | S4 | S5 | S6

-- instance Show SixSidedDice where
--     show S1 = "1"
--     show S2 = "2"
--     show S3 = "3"
--     show S4 = "4"
--     show S5 = "5"
--     show S6 = "6"

-- instance Enum SixSidedDice where
--     toEnum 1 = S1
--     toEnum 2 = S2
--     toEnum 3 = S3
--     toEnum 4 = S4
--     toEnum 5 = S5
--     toEnum 6 = S6
--     toEnum _ = error "No such value"
--     fromEnum S1 = 1
--     fromEnum S2 = 2
--     fromEnum S3 = 3
--     fromEnum S4 = 4
--     fromEnum S5 = 5
--     fromEnum S6 = 6

-- instance Eq SixSidedDice where
--     (==) a b = fromEnum a == fromEnum b

-- instance Ord SixSidedDice where
--     compare a b = fromEnum a `compare` fromEnum b

-- data TwoSidedDice = One | Two

-- show :: TwoSidedDice -> String
-- show One = "1"
-- show Two = "2"

newtype Name = Name (String, String) deriving (Show, Eq)

instance Ord Name where
    compare (Name (fn1, ln1)) (Name (fn2, ln2)) = compare (ln1, fn1) (ln2, fn2)

names :: [Name]
names = [Name ("Emil","Cioran")
        , Name ("Eugene","Thacker")
        , Name ("Friedrich","Nietzsche")]


class (Eq a, Enum a) => Dice a where
    roll :: Int -> a

data FiveSidedDice = S1 | S2 | S3 | S4 | S5 deriving (Eq, Enum, Show)

instance Dice FiveSidedDice where
    roll a = toEnum (a `div` 5)