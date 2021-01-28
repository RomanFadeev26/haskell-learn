import qualified Data.Map as Map

data RobotPart = RobotPart {
    name:: String
    , description :: String
    , cost :: Double
    , count :: Int
} deriving Show

instance Eq RobotPart where
    (==) prt1 prt2 = cost prt1 == cost prt2

instance Ord RobotPart where
    compare fstPart sndPart
        | fstPart == sndPart = EQ
        | cost fstPart > cost sndPart = GT
        | otherwise = LT

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
    , cost = 1021.00
    , count = 5
}

robotHead :: RobotPart
robotHead = RobotPart {
    name = "robot head"
    , description = "this head looks mad"
    , cost = 5092.25
    , count = 2
}

leftLeg = RobotPart {
    name = "left leg"
    , description = "left leg for kind hand gestures"
    , cost = 1025.00
    , count = 5
}

rightLeg = RobotPart {
    name = "right leg"
    , description = "right leg for kind hand gestures"
    , cost = 1026.00
    , count = 0
}

partsDB :: Map.Map Int RobotPart
partsDB = Map.fromList keyVals
    where keys = [1,2,3,4,5]
          vals = [leftArm, robotHead, rightArm, leftLeg, rightLeg]
          keyVals = zip keys vals

compareParts :: RobotPart -> RobotPart -> RobotPart
compareParts fPrt sPrt = if costFPrt > costSPrt
                         then sPrt
                         else fPrt
    where costFPrt = cost fPrt
          costSPrt = cost sPrt

readInt :: IO Int
readInt = read <$> getLine

printPart :: Maybe RobotPart -> IO ()
printPart Nothing = print "part not found"
printPart (Just cost) = print cost

main :: IO ()
main = do
    putStrLn "Enter first part ID"
    id1 <- readInt
    putStrLn "Enter second part ID"
    id2 <- readInt
    print (min <$> Map.lookup id1 partsDB <*> Map.lookup id2 partsDB)