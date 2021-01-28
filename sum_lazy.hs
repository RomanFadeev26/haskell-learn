main :: IO ()
main = do
    userInput <- getContents
    let reversed = reverse userInput
    mapM_ print reversed

sampleData = ['6','2','\n','2','1','\n']

