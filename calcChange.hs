calcChange :: (Num a, Ord a) => a -> a -> a
calcChange owed given
    | change > 0 = change
    | otherwise = 0
    where change = given - owed


inc' :: Num a => a -> a
inc' x = x + 1

double' :: Num a => a -> a
double' x = x * 2

square' :: Num a => a -> a
square' x = x * x

test :: Integral a => a -> a
test x
    | even x = x - 2
    | otherwise = 3 * x + 1