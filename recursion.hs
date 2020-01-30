import Data.Char

gcd' a 0 = a
gcd' a b = gcd' b (a `mod` b)


myTail (_:xs) = xs
myTail [] = error "No tail for empty list"

take' 0 list = list
take' _ [] = []
take' n (x:xs) = x: take (n - 1) xs

drop' 0 list = list
drop' _ [] = []
drop' n (_:xs) = drop' (n - 1) xs

myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myCycle list = list ++ myCycle list

ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

collatz 1 = 1
collatz n = if even n
            then 1 + collatz (n `div` 2)
            else 1 + collatz (n * 3 + 1)

reverse' [] = []
reverse' (x:xs) = (reverse' xs) ++ [x]

fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fastFib n1 _ 0 = n1
fastFib n1 n2 counter = fastFib n2 (n1 + n2) (counter - 1)

map' _ [] = []
map' func (x:xs) = (func x): map' func xs

remove' _ [] = []
remove' test (x:xs) = if (not . test) x
                      then x:remove' test xs
                      else remove' test xs

myProduct list = foldl (*) 1 list

sumOfSquares = (foldl (+) 0).(map (^2))

myFold :: (a -> b -> a) -> a -> [b] -> a
myFold _ initValue [] = initValue
myFold f initValue (x:xs) = myFold f (f initValue x) xs

myFoldR _ initial [] = initial
myFoldR f initial (x:xs) = f x rightInit
    where rightInit = myFoldR f initial xs


myElem a list = length (filter (a==) list) == 1

isPalindrome a = lowerWithoutSpaces a == reverse (lowerWithoutSpaces a)
    where lowerWithoutSpaces = (filter (' '/=)).(map toLower)

harmonic n = sum (map (1/) [1..n])

harmonic2 n = sum (take n seriesValues)
    where seriesPairs = zip (cycle [1.0]) [1.0,2.0 .. ]
          seriesValues = map (\pair -> (fst pair)/(snd pair)) seriesPairs