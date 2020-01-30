x :: Int
x = 2

y :: Integer
y = 2

letter :: Char
letter = 'a'

interestRate :: Double
interestRate = 3.75

isFun :: Bool
isFun = True

half :: Int -> Double
half a = fromIntegral a / 2

halve :: Int -> Int
halve a = a `div` 2

printDouble :: Int -> String
printDouble a = show (a * 2)

anotherNumber :: Int
anotherNumber = read "6"

makeAddress :: Int -> String -> String -> (Int, String, String)
makeAddress house street city = (house, street, city)

simple :: a -> a
simple a = a

makeTriple :: a -> b -> c -> (a,b,c)
makeTriple a b c = (a, b, c)

myTail :: [a] -> [a]
myTail [] = []
myTail (_:xs) = xs
