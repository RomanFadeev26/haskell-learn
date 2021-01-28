{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

sampleBytes :: B.ByteString
sampleBytes = "Hello!"

-- sampleString :: String
-- sampleString = B.unpack sampleBytes

convertToInt :: BC.ByteString -> Int
convertToInt = read . BC.unpack

testBytes :: BC.ByteString
testBytes = "431"
