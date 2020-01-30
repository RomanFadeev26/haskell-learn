sumSquareOrSquareSum :: (Ord p, Num p) => p -> p -> p
sumSquareOrSquareSum x y
    | sumSqr > square = sumSqr
    | otherwise = square
    where square = x ^ 2 + y ^ 2
          sumSqr = (x + y) ^ 2

sumSquareOrSquareSum' x y = (\sumSqr square ->
    if sumSqr > square
    then sumSqr
    else square) (x ^ 2 + y ^ 2) ((x + y) ^ 2)

doubleDouble x = (\dubs -> dubs *2) (x * 2)

sumSquareOrSquareSum'' x y = let sumSqr = (x ^ 2 + y ^ 2)
                                 squareSum = (x + y) ^ 2
                             in
                                if sumSqr > squareSum
                                then sumSqr
                                else squareSum

owerwrite x = let x = 2
                in
                 let x = 3
                  in
                   let x = 4
                    in x

owerwrite' x = (\x -> 4) ((\x -> 3) ((\x -> 2) x))

counter x = (\x -> x + 1) ((\x -> x +1) x)
