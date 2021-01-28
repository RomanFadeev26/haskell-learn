import qualified Data.Map as Map

areaGivenDiameter :: Double -> Double
areaGivenDiameter diameter = pi * (diameter / 2) ^ 2

type Pizza = (Double, Double)

costPerInch :: Pizza -> Double
costPerInch (size, cost)  = cost / (areaGivenDiameter size)

comparePizza :: Pizza -> Pizza -> Pizza
comparePizza p1 p2 = if costPI1 < costPI2
                     then p1
                     else p2
    where costPI1 = costPerInch p1
          costPI2 = costPerInch p2

describePizza :: Pizza -> String
describePizza (size, cost) = "The " ++ show size ++ " pizza is cheaper at " ++ show costSqInch ++ " per square inch"
    where costSqInch = costPerInch (size, cost)

main :: IO ()
main = do
    putStrLn "What is the size of pizza 1"
    size1 <- getLine
    putStrLn "What is the cost of pizza 1"
    cost1 <- getLine
    putStrLn "What is the size of pizza 2"
    size2 <- getLine
    putStrLn "What is the cost of pizza 2"
    cost2 <- getLine
    let pizza1 = (read size1, read cost1)
        pizza2 = (read size2, read cost2)
        betterPizza = comparePizza pizza1 pizza2
    putStrLn (describePizza betterPizza)

-- getPizza :: String -> IO Pizza
-- getPizza n = do
--     putStrLn ("What is the size of pizza " ++ n)
--     size1 <- getLine
--     putStrLn ("What is the cost of pizza " ++ n)
--     cost1 <- getLine
--     return (read size1, read cost1)


mainNotDo :: IO ()
mainNotDo = getPizza "1" >>= (\pizza1 ->
                                    getPizza "2" >>=
                                        (\pizza2 -> return (describePizza $ comparePizza pizza1 pizza2)) >>= putStrLn)
        where getPizza n = putStrLn ("What is the size of pizza " ++ n) >> getLine >>= (\size ->
                            putStrLn ("What is the cost of pizza " ++ n) >> getLine >>= (\cost ->
                                return (read size, read cost)))

costData :: Map.Map Int Double
costData = Map.fromList [(1, 10.0), (2, 20.0)]

sizeData :: Map.Map Int Double
sizeData = Map.fromList [(1, 15.0), (2, 23.0)]

costDataList1 :: [Double]
costDataList1 = [10.0, 20.0]

sizeDataList1 :: [Double]
sizeDataList1 = [15.0, 23.0]

costDataList2 :: [Double]
costDataList2 = [20.0, 30.0]

sizeDataList2 :: [Double]
sizeDataList2 = [35.0, 33.0]

maybeMain :: Maybe String
maybeMain = do
    size1 <- Map.lookup 1 sizeData
    cost1 <- Map.lookup 1 costData
    size2 <- Map.lookup 2 sizeData
    cost2 <- Map.lookup 2 costData
    let pizza1 = (size1, cost1)
        pizza2 = (size2, cost2)
        betterPizza = comparePizza pizza1 pizza2
    return (describePizza betterPizza)

maybeNotDoMain :: Maybe String
maybeNotDoMain = getSize 1 >>= (\size1 -> getCost 1 >>=
                    (\cost1 -> getSize 2 >>=
                        (\size2 -> getCost 2 >>=
                            (\cost2 ->
                                (\pizza1 ->
                                    (return . describePizza) (size2, cost2) ) (size1, cost1)))))
    where getSize id = Map.lookup id sizeData
          getCost id = Map.lookup id costData

listMain :: [String]
listMain = do
    size1 <- sizeDataList1
    cost1 <- costDataList1
    size2 <- sizeDataList2
    cost2 <- costDataList2
    let pizza1 = (size1, cost1)
        pizza2 = (size2, cost2)
        betterPizza = comparePizza pizza1 pizza2
    return (describePizza betterPizza)

monadMain :: Monad m => m Double -> m Double -> m Double -> m Double -> m String
monadMain msize1 mcost1 msize2 mcost2 = do
    size1 <- msize1
    cost1 <- mcost1
    size2 <- msize2
    cost2 <- mcost2
    let pizza1 = (size1, cost1)
        pizza2 = (size2, cost2)
        betterPizza = comparePizza pizza1 pizza2
    return (describePizza betterPizza)