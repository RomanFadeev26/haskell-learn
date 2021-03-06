import qualified Data.Map as Map

type UserName = String
type GamerId = Int
type PlayerCredits = Int

userNameDB :: Map.Map GamerId UserName
userNameDB = Map.fromList [(1,"nYarlathoTep")
                          , (2,"KINGinYELLOW")
                          , (3,"dagon1997")
                          , (4,"rcarter1919")
                          , (5,"xCTHULHUx")
                          , (6,"yogSOThoth")]

creditsDB :: Map.Map UserName PlayerCredits
creditsDB = Map.fromList [("nYarlathoTep",2000)
                         , ("KINGinYELLOW",15000)
                         , ("dagon1997",300)
                         , ("rcarter1919",12)
                         , ("xCTHULHUx",50000)
                         , ("yogSOThoth",150000)]

lookupUserName :: GamerId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDB

lookupCredits :: UserName -> Maybe PlayerCredits
lookupCredits userName = Map.lookup userName creditsDB

altLookupCredits :: Maybe UserName -> Maybe PlayerCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just userName) = lookupCredits userName

-- creditsFromId :: GamerId -> Maybe PlayerCredits
-- creditsFromId id = altLookupCredits (lookupUserName id)

creditsFromId :: GamerId -> Maybe PlayerCredits
creditsFromId id = lookupUserName id >>= lookupCredits

type WillCoId = Int

gamerIdDB :: Map.Map WillCoId GamerId
gamerIdDB = Map.fromList [ (1001,1)
                         , (1002,2)
                         , (1003,3)
                         , (1004,4)
                         , (1005,5)
                         , (1006,6)]

lookupGamerId :: WillCoId -> Maybe GamerId
lookupGamerId id = Map.lookup id gamerIdDB

creditsFromWCId :: WillCoId -> Maybe PlayerCredits
creditsFromWCId id = lookupGamerId id >>= lookupUserName >>= lookupCredits

echo :: IO ()
echo = getLine >>= putStrLn

readInt :: IO Int
readInt = read <$> getLine

printDouble :: Int -> IO ()
printDouble a = print (a * 2)

doublePrint :: IO ()
doublePrint = readInt >>= printDouble

echoVerbose :: IO ()
echoVerbose = putStrLn "Enter a String an we'll echo it!" >> getLine >>= putStrLn

askForName :: IO ()
askForName = putStrLn "What is your name?"

nameStatement :: String -> String
nameStatement name = mconcat ["Hello, ", name, "!"]

helloName :: IO ()
helloName = askForName >> getLine >>= return . nameStatement >>= putStrLn

allFmapM :: Monad m => (a -> b) -> m a -> m b
allFmapM f m = m >>= return . f

allApp :: Monad m => m (a -> b) -> m a -> m b
allApp fm a = fm >>= (<$> a)

bind :: Maybe a -> (a -> Maybe b) -> Maybe b
bind Nothing _ = Nothing
bind (Just a) f = f a

