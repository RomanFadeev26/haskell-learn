{-# LANGUAGE ConstrainedClassMethods #-}

import Data.List
import qualified Data.Map as Map
import Data.Semigroup
import Data.Maybe

file1 :: [(Int,Double)]
file1 = [ (1, 200.1), (2, 199.5), (3, 199.4)
        , (4, 198.9), (5, 199.0), (6, 200.2)
        , (9, 200.3), (10, 201.2), (12, 202.9)]

file2 :: [(Int,Double)]
file2 = [(11, 201.6), (12, 201.5), (13, 201.5)
        ,(14, 203.5), (15, 204.9), (16, 207.1)
        ,(18, 210.5), (20, 208.8)]

file3 :: [(Int,Double)]
file3 = [(10, 201.2), (11, 201.6), (12, 201.5)
        ,(13, 201.5), (14, 203.5), (17, 210.5)
        ,(24, 215.1), (25, 218.7)]

file4 :: [(Int,Double)]
file4 = [(26, 219.8), (27, 220.5), (28, 223.8)
    ,(29, 222.8), (30, 223.8), (31, 221.7)
    ,(32, 222.3), (33, 220.8), (34, 219.4)
    ,(35, 220.1), (36, 220.6)]

data TS a = TS [Int] [Maybe a]

class ComputableTS a where 
    add :: Num a => TS a -> TS a -> TS a

createTS :: [Int] -> [a] -> TS a
createTS times values = TS completeTimes extendedValues
                        where completeTimes = [minimum times .. maximum times]
                              timeValuesMap = Map.fromList (zip times values)
                              extendedValues = map (\v -> Map.lookup v timeValuesMap) completeTimes

fileToTs :: [(Int, Double)] -> TS Double
fileToTs tsPairs = createTS times values
                   where (times, values) = unzip tsPairs

showTVPairs :: Show a => Int -> Maybe a -> String
showTVPairs time (Just value) = mconcat [show time, "|", show value, "\n"]
showTVPairs time Nothing = mconcat [show time, "|NA\n"]

instance Show a => Show (TS a) where
    show (TS times values) = mconcat rows
                             where rows = zipWith showTVPairs times values


ts1 :: TS Double
ts1 = fileToTs file1

ts2 :: TS Double
ts2 = fileToTs file2

ts3 :: TS Double
ts3 = fileToTs file3

ts4 :: TS Double
ts4 = fileToTs file4


insertMaybePair :: Ord k => Map.Map k v -> (k, Maybe v) -> Map.Map k v
insertMaybePair myMap (_, Nothing) = myMap
insertMaybePair myMap (k, (Just v)) = Map.insert k v myMap

combineTS :: TS a -> TS a -> TS a
combineTS (TS [] []) ts2 = ts2
combineTS ts1 (TS [] []) = ts1
combineTS (TS t1 v1) (TS t2 v2) = TS completeTimes combinedValues
    where bothTimes = mconcat [t1, t2]
          completeTimes = [minimum bothTimes .. maximum bothTimes]
          tvMap = foldl insertMaybePair Map.empty (zip t1 v1)
          updatedMap = foldl insertMaybePair tvMap (zip t2 v2)
          combinedValues = map (\t -> Map.lookup t updatedMap) completeTimes


instance Semigroup (TS a) where
    (<>) = combineTS
instance Monoid (TS a) where
    mempty = TS [] []
    mappend = (<>)

tsAll :: TS Double
tsAll = mconcat [ts1, ts2, ts3, ts4]

mean :: (Real a) => [a] -> Double
mean xs = total/count
    where total = (realToFrac . sum) xs
          count = (realToFrac . length) xs

meanTS :: (Real a) => TS a -> Maybe Double
meanTS (TS _ []) = Nothing
meanTS (TS times values) = if all (==Nothing) values
                           then Nothing
                           else Just average
    where justVals = filter isJust values
          cleanValues = map fromJust justVals
          average = mean cleanValues


type CompareFunc a = a -> a -> a
type CompareFuncTS a = (Int, Maybe a) -> (Int, Maybe a) -> (Int, Maybe a)

makeTSCompare :: Eq a => CompareFunc a -> CompareFuncTS a
makeTSCompare func = newFunc
    where newFunc (i1, Nothing) (i2, Nothing) = (i1, Nothing)
          newFunc (_, Nothing) (i, val) = (i, val)
          newFunc (i, val) (_, Nothing) = (i, val)
          newFunc (i1, (Just val1)) (i2, (Just val2)) =
                                                       if (func val1 val2) == val1
                                                       then (i1, (Just val1))
                                                       else (i2, (Just val2))

compareTS :: Eq a => (a -> a -> a) -> TS a -> Maybe (Int, Maybe a)
compareTS _ (TS [] []) = Nothing
compareTS func (TS times values) = if all (==Nothing) values
                                   then Nothing
                                   else Just best
    where pairs = zip times values
          best = foldl (makeTSCompare func) (0, Nothing) pairs



minTS :: Ord a => TS a -> Maybe (Int, Maybe a)
minTS = compareTS min

maxTS :: Ord a => TS a -> Maybe (Int, Maybe a)
maxTS = compareTS max

diffPair :: Num a => Maybe a -> Maybe a -> Maybe a
diffPair Nothing _ = Nothing
diffPair a Nothing = a
diffPair (Just x) (Just y) = Just (x - y)

diffTS :: Num a => TS a -> TS a
diffTS (TS [] []) = TS [] []
diffTS (TS times values) = TS times (Nothing:diffValues)
    where shiftedValues = tail values
          diffValues = zipWith diffPair shiftedValues values

meanMaybe :: Real a => [Maybe a] -> Maybe Double
meanMaybe vals = if anyIsNothing
                 then Nothing
                 else (Just meanValue)
    where anyIsNothing = any (==Nothing) vals
          meanValue = mean $ map fromJust vals

movingAverage :: Real a => [Maybe a] -> Int -> [Maybe Double]
movingAverage [] _ = []
movingAverage vals n = if (length nextVals) == n
                       then meanMaybe nextVals:movingAverage restVals n
                       else []
    where nextVals = take n vals
          restVals = tail vals

movingAverageTS :: Real a => TS a -> Int -> TS Double
movingAverageTS (TS [] []) _ = TS [] []
movingAverageTS (TS times values) n = TS times averagedValues
    where ma = movingAverage values n
          nothings = replicate (n `div` 2) Nothing
          averagedValues = mconcat [nothings, ma, nothings]

addMaybe :: Num a => Maybe a -> Maybe a -> Maybe a
addMaybe Nothing b = b
addMaybe a Nothing = a
addMaybe (Just a) (Just b) = Just (a + b)

addTS :: Num a => TS a -> TS a -> TS a
addTS (TS [] []) b = b
addTS a (TS [] []) = a
addTS (TS times1 values1) (TS times2 values2) = TS allTimes computedValues
    where allMixedTimes = times1 <> times2
          allTimes = [minimum allMixedTimes .. maximum allMixedTimes]
          filtered1 = filter (\(_, v) -> isJust v) (zip times1 values1)
          mapped1 = map (\(t, v) -> (t, fromJust v)) filtered1
          filtered2 = filter (\(_, v) -> isJust v) (zip times2 values2)
          mapped2 = map (\(t, v) -> (t, fromJust v)) filtered2
          ts1Map = Map.fromList mapped1
          ts2Map = Map.fromList mapped2
          computedValues = map (\t -> addMaybe (Map.lookup t ts1Map) (Map.lookup t ts2Map)) allTimes

substractTS :: Num a => TS a -> TS a -> TS a
substractTS (TS [] []) b = TS [] []
substractTS a (TS [] []) = a
substractTS (TS times1 values1) (TS times2 values2) = TS allTimes computedValues
    where allMixedTimes = times1 <> times2
          allTimes = [minimum allMixedTimes .. maximum allMixedTimes]
          filtered1 = filter (\(_, v) -> isJust v) (zip times1 values1)
          mapped1 = map (\(t, v) -> (t, fromJust v)) filtered1
          filtered2 = filter (\(_, v) -> isJust v) (zip times2 values2)
          mapped2 = map (\(t, v) -> (t, fromJust v)) filtered2
          ts1Map = Map.fromList mapped1
          ts2Map = Map.fromList mapped2
          computedValues = map (\t -> diffPair (Map.lookup t ts1Map) (Map.lookup t ts2Map)) allTimes

instance ComputableTS (TS a) where
   add = addTS
--  substract = substractTS
