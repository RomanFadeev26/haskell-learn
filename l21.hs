import qualified Data.Map as Map

helloPerson :: String -> String
helloPerson person = "Hello, " ++ person ++ "!"

main :: IO ()
main = do
    putStrLn "Hello! What's your name?"
    name <- getLine
    let statement = helloPerson name
    putStrLn statement
    
persons :: Map.Map Int String
persons = Map.fromList [(1, "David")]

mainMaybe :: Maybe String
mainMaybe = do
    name <- Map.lookup 1 persons
    return (helloPerson name)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

mainFib :: IO Int
mainFib = do
    putStrLn "Write fibonacci number"
    n <- getLine
    let result = fib (read n)
    return result