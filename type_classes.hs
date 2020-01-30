addThenDouble :: Num a => a -> a -> a
addThenDouble a b = (a + b) * 2

class Describable a where
    describe :: a -> String

data Icecream = Vanilla | Chocolate deriving (Show, Eq, Ord)

inc :: Int -> Int
inc a = a + 1

cycleSucc :: (Bounded a, Enum a, Eq a) => a -> a
cycleSucc n = if n == maxBound
              then minBound
              else succ n