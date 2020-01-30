myAll :: (a -> Bool) -> [a] -> Bool
myAll predicate = (foldr (&&) True) . (map predicate)

myAny :: (a -> Bool) -> [a] -> Bool
myAny predicate = (foldl (||) False) . (map predicate)

data Color = Red
    | Yellow
    | Blue
    | Green
    | Purple
    | Orange
    | Transparent
    | Brown deriving (Show, Eq)

instance Semigroup Color where
    (<>) Red Blue = Purple
    (<>) Blue Red = Purple
    (<>) Yellow Blue = Green
    (<>) Blue Yellow = Green
    (<>) Yellow Red = Orange
    (<>) Red Yellow = Orange
    (<>) a b | a == b = a
             | all (`elem` [Red, Blue, Purple]) [a, b] = Purple
             | all (`elem` [Blue, Yellow, Green]) [a, b] = Green
             | all (`elem` [Red, Yellow, Orange]) [a, b] = Orange
             | a == Transparent = b
             | b == Transparent = a
             | otherwise = Brown

instance Monoid Color where
    mappend = (<>)
    mempty = Transparent

data Events = Events [String] deriving (Eq, Show)

instance Semigroup Events where
    (<>) = combineEvents

instance Monoid Events where
    mappend = (<>)
    mempty = (Events [])

data Probs = Probs [Double] deriving (Eq, Show)

instance Semigroup Probs where
    (<>) = combineProbs

instance Monoid Probs where
    mappend = (<>)
    mempty = (Probs [])

showPair :: String -> Double -> String
showPair event prob = mconcat [event, "|", show prob, "\n"]

data PTable = PTable Events Probs deriving Eq

instance Show PTable where
    show (PTable (Events events) (Probs probs)) = mconcat pairs
                                    where pairs = zipWith showPair events probs

instance Semigroup PTable where
    (<>) pTable1 (PTable (Events []) (Probs [])) = pTable1
    (<>) (PTable (Events []) (Probs [])) pTable2 = pTable2
    (<>) (PTable events1 probs1) (PTable events2 probs2) = createPTable computedEvents computedProbs
                                                            where computedEvents = combineEvents events1 events2
                                                                  computedProbs = combineProbs probs1 probs2

instance Monoid PTable where
    mappend = (<>)
    mempty = (PTable (Events []) (Probs []))
    

createPTable :: Events -> Probs -> PTable
createPTable (Events events) (Probs probs) = PTable (Events events) (Probs normalizedProbs)
                            where totalProbs = sum probs
                                  normalizedProbs = map (\x -> x/totalProbs) probs

cartCombine :: (a -> b -> c) -> [a] -> [b] -> [c]
cartCombine func listA listB = zipWith func newLA cycledLB
                                where nToAdd = length listB
                                      repeatedLA = map (take nToAdd . repeat) listA
                                      newLA = mconcat repeatedLA
                                      cycledLB = cycle listB
                                      
combineEvents :: Events -> Events -> Events
combineEvents (Events events1) (Events events2) = Events (cartCombine (\x y -> mconcat [x, "-", y]) events1 events2)

combineProbs :: Probs -> Probs -> Probs
combineProbs (Probs probs1) (Probs probs2) = Probs (cartCombine (*) probs1 probs2)

coin :: PTable
coin = createPTable (Events ["head", "tail"]) (Probs [0.5, 0.5])

spinner :: PTable
spinner = createPTable (Events ["red", "blue", "green"]) (Probs [0.1, 0.2, 0.7])

