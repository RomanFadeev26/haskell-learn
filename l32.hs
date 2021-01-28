import Control.Monad
import Data.Char

powersOfTwo :: Int -> [Int]
powersOfTwo n = do
    value <- [1 .. n]
    return (2 ^ value)

powersOfTwoAndThree :: Int -> [(Int, Int)]
powersOfTwoAndThree n = do
    value <- [1 .. n]
    let powersOfTwo = 2 ^ value
        powersOfThree = 3 ^ value
    return (powersOfTwo, powersOfThree)

allEvenOdds :: Int -> [(Int, Int)]
allEvenOdds n = do
    evenValue <- [2, 4 .. n]
    oddValue <- [1, 3 .. n]
    return (evenValue, oddValue)

numbersAndTheirSquares :: Int -> [(Int, Int)]
numbersAndTheirSquares n = do
    value <- [1..n]
    return (value, value ^ 2)

evensGuard :: Int -> [Int]
evensGuard n = do
    value <- [1..n]
    guard $ even value
    return value

doFilter :: (a -> Bool) -> [a] -> [a]
doFilter f l = do
    val <- l
    guard $ f val
    return val

powersOfTwoLC :: Int -> [Int]
powersOfTwoLC n = [value ^ 2 | value <- [1..n]]

powersOfTwoAndThreeLC :: Int -> [(Int, Int)]
powersOfTwoAndThreeLC n = [(powersOfTwo, powersOfThree)
                          | value <- [1..n]
                          , let powersOfTwo = 2 ^ value
                          , let powersOfThree = 3 ^ value ]

allEvenOddsLC :: Int -> [(Int, Int)]
allEvenOddsLC n = [(evenValue, oddValue)
                  | evenValue <- [2,4..n]
                  , oddValue <- [1,3..n]]

evensGuardLC :: Int -> [Int]
evensGuardLC n = [value
                 | value <- [1..n], even value]

colors :: [String]
colors = ["brown","blue","pink","orange"]

mens :: [String]
mens = [ "Mr. " ++ val | firstChar:rest <- colors, let val = toUpper firstChar : rest]

longMonths :: [Int]
longMonths = [1, 3, 5, 7, 8, 10, 12]

daysInMonth :: Int -> [Int]
daysInMonth month
    | month `elem` longMonths = [1..31]
    | month == 2 = [1..28]
    | otherwise = [1..30]

type Calendar = [[Int]]

calendar :: Calendar
calendar = [days
            | month <- [1..12]
            , let days = daysInMonth month]

doCalendar :: Calendar
doCalendar = do
    month <- [1..12]
    return (daysInMonth month)

mCalendar :: Calendar
mCalendar = [1..12] >>= return . daysInMonth
