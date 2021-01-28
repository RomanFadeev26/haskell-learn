{-# LANGUAGE OverloadedStrings #-}

import System.IO
import System.Environment
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

getCounts :: T.Text -> (Int, Int, Int)
getCounts input = (charCount, wordCount, lineCount)
    where charCount = T.length input
          wordCount = (length . T.words) input
          lineCount = (length . T.lines) input

countsText :: (Int, Int, Int) -> T.Text
countsText (charsCount, wordsCount, linesCount) = T.pack $ unwords ["chars: ", show charsCount, " words: ", show wordsCount, " lines: ", show linesCount]

main :: IO ()
main = do
    args <- getArgs
    let fileName = head args
    input <- TIO.readFile fileName
    let summary = (countsText . getCounts) input
    TIO.appendFile "stats.dat" (mconcat [T.pack fileName, " ", summary, "\n"])
    TIO.putStrLn summary

-- main :: IO ()
-- main = do
--     args <- getArgs
--     let fileName = head args
--     fileHandler <- openFile fileName ReadMode
--     input <- hGetContents fileHandler
--     let summary = (countsText . getCounts) input
--     putStrLn summary
--     hClose fileHandler
--     appendFile "stats.dat" (mconcat [fileName, " ", summary, "\n"])