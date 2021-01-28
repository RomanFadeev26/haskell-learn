hello :: IO String
hello = pure "Hello"

doorPrize :: [Int]
doorPrize = [1000, 2000, 3000]

boxPrize :: [Int]
boxPrize = [10, 50]

totalPrize :: [Int]
totalPrize = pure (*) <*> doorPrize <*> boxPrize

primesToN :: Int -> [Int]
primesToN n = filter isNoComposite twoThroughN
    where twoThroughN = [2..n]
          composites = pure (*) <*> twoThroughN <*> twoThroughN
          isNoComposite = not . (`elem` composites)

data User = User
    { name:: String
    , gamerId :: Int
    , score :: Int
    } deriving Show

testNames :: [String]
testNames = ["John Smith"
            ,"Robert'); DROP TABLE Students;--"
            ,"Christina NULL"
            ,"Randall Munroe"]

testIds :: [Int]
testIds = [1337
          ,0123
          ,999999]

testScores :: [Int]
testScores = [0
             ,100000
             ,-99999]

testData :: [User]
testData = pure User <*> testNames <*> testIds <*> testScores

allFmap :: Applicative f => (a -> b) -> f a -> f b
allFmap func arg1 = func <$> arg1

exampleMaybe :: Maybe Int
exampleMaybe = pure (*) <*> (pure (+) <*> (pure 2) <*> (pure 4)) <*> (pure 6)

packsOfBeer :: [Int]
packsOfBeer = [6, 12]

lastNightWeDrunk :: [Int]
lastNightWeDrunk = [4]

friendsWillCome :: [Int]
friendsWillCome = [2, 3]

expectedDrink :: [Int]
expectedDrink = [3, 4]

beerLeft :: [Int]
beerLeft = (-) <$> packsOfBeer <*> lastNightWeDrunk

buddiesWillDrink :: [Int]
buddiesWillDrink = pure (*) <*> friendsWillCome <*> expectedDrink

shouldBought :: Int
shouldBought = maximum $ pure (-) <*> buddiesWillDrink <*> beerLeft