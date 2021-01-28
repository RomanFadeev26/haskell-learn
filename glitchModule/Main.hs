module Main where

import System.Environment

import qualified Data.ByteString.Char8 as BC

import Glitch

main :: IO ()
main = do
    fileName:_ <- getArgs
    imageFile <- BC.readFile fileName
    glitched <- randomReverseSection imageFile
    let glitchedFileName = mconcat ["glitched_", fileName]
    BC.writeFile glitchedFileName glitched
    print "all done!"

-- glitchActions :: BC.ByteString -> IO BC.ByteString
-- glitchActions imageFile = randomReplaceByte imageFile >>=
--     randomSortSection >>= randomReplaceByte >>= randomSortSection >>= randomReplaceByte


-- replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
-- replaceByte loc charVal bytes = mconcat [before, newChar, after]
--     where (before, rest) = BC.splitAt loc bytes
--           after = BC.drop 1 rest
--           newChar = intToBC charVal

-- randomReplaceByte :: BC.ByteString -> IO BC.ByteString
-- randomReplaceByte bytes = do
--     let bytesLength = BC.length bytes
--     location <- randomRIO (1, bytesLength)
--     charVal <- randomRIO (0, 255)
--     return (replaceByte location charVal bytes)

-- randomChar :: IO Char
-- randomChar = do
--     randInt <- randomRIO (0, 255)
--     return $ toEnum randInt


-- randomSortSection :: BC.ByteString -> IO BC.ByteString
-- randomSortSection = randomChangeSection sortSection
