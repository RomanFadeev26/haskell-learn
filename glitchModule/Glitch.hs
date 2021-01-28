module Glitch (randomReverseSection) where

import System.Random
import qualified Data.ByteString.Char8 as BC

intToChar :: Int -> Char
intToChar int = toEnum safeInt
    where safeInt = int `mod` 255

intToBC :: Int -> BC.ByteString
intToBC int = BC.pack [intToChar int]

changeSection :: (BC.ByteString -> BC.ByteString) -> Int -> Int -> BC.ByteString -> BC.ByteString
changeSection func start size bytes = mconcat [before, changed, after]
    where (before, rest) = BC.splitAt start bytes
          (target, after) = BC.splitAt size rest
          changed = func target

sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection = changeSection (BC.reverse . BC.sort)

randomChangeSection :: (Int -> Int -> BC.ByteString -> BC.ByteString) -> BC.ByteString -> IO BC.ByteString
randomChangeSection func bytes = do
    let sectionSize = 25
    let bytesLength = BC.length bytes
    start <- randomRIO (0, bytesLength - sectionSize)
    return (func start sectionSize bytes)

reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection = changeSection BC.reverse

randomReverseSection :: BC.ByteString -> IO BC.ByteString
randomReverseSection = randomChangeSection reverseSection