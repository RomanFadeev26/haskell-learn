import System.IO

-- main :: IO ()
-- main = do
--     helloFile <- openFile "./hello.txt" ReadMode
--     firstLine <- hGetLine helloFile
--     putStrLn firstLine
--     secondLine <- hGetLine helloFile
--     goodByeFile <- openFile "./goodbye.txt" WriteMode
--     hPutStrLn goodByeFile secondLine
--     hClose helloFile
--     hClose goodByeFile
--     putStrLn "done!"

handleLastLine :: Handle -> String -> IO String
handleLastLine handle fallback = do
    lastLine <- hIsEOF handle
    if not lastLine
    then hGetLine handle
    else return fallback

main :: IO ()
main = do
    helloFile <- openFile "./hello.txt" ReadMode
    firstLine <- handleLastLine helloFile "empty"
    secondLine <- handleLastLine helloFile ""
    goodByeFile <- openFile "./goodbye.txt" WriteMode
    hPutStrLn goodByeFile secondLine
    hClose helloFile
    hClose goodByeFile
    putStrLn "done!"