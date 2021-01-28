import qualified Data.Map as Map

reverseMaybe :: Maybe String -> Maybe String
reverseMaybe Nothing = Nothing
reverseMaybe (Just a) = Just (reverse a)

data RobotPart = RobotPart {
    name:: String
    , description :: String
    , cost :: Double
    , count :: Int
} deriving Show

leftArm :: RobotPart
leftArm = RobotPart {
    name = "left arm"
    , description = "left arm for face punching!"
    , cost = 1000.0
    , count = 3
}

rightArm :: RobotPart
rightArm = RobotPart {
    name = "right arm"
    , description = "right arm for kind hand gestures"
    , cost = 1025.00
    , count = 5
}

robotHead :: RobotPart
robotHead = RobotPart {
    name = "robot head"
    , description = "this head looks mad"
    , cost = 5092.25
    , count = 2
}

type Html = String

renderHtml :: RobotPart -> Html
renderHtml part = mconcat [
    "<h2>"
    , partName
    , "</h2>"
    , "<p><h3> desc </h3>"
    , partDesc
    , "</p><p><h3> cost </h3>"
    , partCost
    , "</p><p><h3>count</h3>"
    , partCount
    , "</p>"]
    where partName = name part
          partDesc = description part
          partCost = show $ cost part
          partCount = show $ count part

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
    where keys = [1,2,3]
          vals = [leftArm, robotHead, rightArm]
          keyVals = zip keys vals

-- insertSnippet :: Maybe Html -> IO ()

partVal :: Maybe RobotPart
partVal = Map.lookup 1 partsDB

partHtml :: Maybe Html
partHtml = renderHtml <$> partVal

allParts :: [RobotPart]
allParts = snd <$> Map.toList partsDB

allPartsHtml :: [Html]
allPartsHtml = renderHtml <$> allParts

htmlPartsDB :: Map.Map Int Html
htmlPartsDB = renderHtml <$> partsDB

leftArmIO :: IO RobotPart
leftArmIO = return leftArm

htmlSnippet :: IO Html
htmlSnippet = renderHtml <$> leftArmIO

newtype Box a = Box a deriving Show

instance Functor Box where
    fmap f (Box a) = Box (f a)

morePresents :: Box a -> Int -> Box [a]
morePresents box n = replicate n <$> box

myBox :: Box Int
myBox = Box 1

wrapped :: Box (Box Int)
wrapped = Box <$> myBox

unwrap :: Box a -> a
unwrap (Box box) = box

printCost :: Maybe Double -> IO ()
printCost Nothing = print "item not found"
printCost (Just cost) = print cost

main :: IO ()
main = do
    line <- getLine
    let part = Map.lookup (read line) partsDB
    printCost (cost <$> part)