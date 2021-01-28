{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Data.Semigroup

firstWord :: String
firstWord = "optimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

cheese :: T.Text
cheese = "CHEESE"

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

someText :: T.Text
someText = "Some\ntext for\t you"

breakText :: T.Text
breakText = "simple"

exampleText :: T.Text
exampleText = "This is simple to do"

combinedTextMonoid :: T.Text
combinedTextMonoid = mconcat ["some", " text"]

combinedTextSemigroup :: T.Text
combinedTextSemigroup = "some" <> " text"

tLines :: T.Text -> [T.Text]
tLines = T.splitOn "\n"

tUnlines :: [T.Text] -> T.Text
tUnlines = T.intercalate "\n"

