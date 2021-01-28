{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as E
import Data.Maybe

type Author = T.Text
type Title = T.Text
type Html = T.Text

data Book = Book {
    author :: Author
    , title :: Title}

bookToHtml :: Book -> Html
bookToHtml book = mconcat ["<p>\n"
                          , titleInTags
                          , authorInTags
                          , "</p>\n"]
    where titleInTags = mconcat ["<strong>",title book, "</strong>\n"]
          authorInTags = mconcat ["<em>", author book, "</em>\n"]

book1 :: Book
book1 = Book {
    author = "Ligotti, Thomas"
    , title = "The Conspiracy Against the Human Race"}

book2 :: Book
book2 = Book {
    author = "Cioran, Emil"
    , title = "A Short History of Decay"}

book3 :: Book
book3 = Book {
    author = "Bataille, Georges"
    , title = "The Tears of Eros"}

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat [
    "<html>\n"
    , "<head>"
    , "<title>books</title>"
    , "<meta charset='utf-8' />"
    , "</head>\n"
    , "<body>\n"
    , booksToHtml
    , "\n</body>\n"
    , "</html>"]
    where booksToHtml = (mconcat . map bookToHtml) books

-- myBooks :: [Book]
-- myBooks = [book1, book2, book3]

-- main :: IO ()
-- main = TIO.writeFile "./html/books.html" (booksToHtml myBooks)

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString
type MarcDirectoryRaw = B.ByteString
type MarcDirectoryEntryRaw = B.ByteString

leaderLength :: Int
leaderLength = 24

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader = B.take leaderLength

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength = rawToInt . B.take 5

nextAndRest :: B.ByteString -> (MarcLeaderRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
    where recordLength = getRecordLength marcStream

allRecords :: B.ByteString -> [MarcRecordRaw]
allRecords marcStream = if marcStream == B.empty
                       then []
                       else next : allRecords rest
    where (next, rest) = nextAndRest marcStream

-- main :: IO ()
-- main = do
--     marcData <- B.readFile "./samples/ohsu_ncnm_wscc_bibs.mrc"
--     let marcRecords = allRecords marcData
--     print (length marcRecords)

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress = rawToInt . B.take 5 . B.drop 12

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - (leaderLength + 1)

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory record = B.take directoryLength cuttedRecord
    where leader = getLeader record
          cuttedRecord = B.drop leaderLength record
          directoryLength = getDirectoryLength leader

dirEntryLength :: Int
dirEntryLength = 12

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory = if directory == B.empty
                           then []
                           else nextEntry : splitDirectory restEntries
    where (nextEntry, restEntries) = B.splitAt dirEntryLength directory

data FieldMetadata = FieldMetadata { tag           :: T.Text
                                     , fieldLength :: Int
                                     , fieldStart  :: Int } deriving Show

makeFieldMetadata :: MarcDirectoryEntryRaw -> FieldMetadata
makeFieldMetadata entry = FieldMetadata textTag theLength theStart
    where (theTag, rest) = B.splitAt 3 entry
          textTag = E.decodeUtf8 theTag
          (rawLength, rawStart) = B.splitAt 4 rest
          theLength = rawToInt rawLength
          theStart = rawToInt rawStart

getFieldMetadata :: [MarcDirectoryEntryRaw] -> [FieldMetadata]
getFieldMetadata = map makeFieldMetadata

type FieldText = T.Text

getTextField :: MarcRecordRaw -> FieldMetadata -> FieldText
getTextField record metadata = E.decodeUtf8 fieldBytes
    where leader = getLeader record
          directoryLength = getDirectoryLength leader
          fieldStartInBytes = fieldStart metadata
          baseRecord = B.drop (leaderLength + directoryLength + fieldStartInBytes) record
          fieldBytes = B.take (fieldLength metadata) baseRecord

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubfield :: Char
titleSubfield = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubfield :: Char
authorSubfield = 'a'

lookupFieldMetadata :: T.Text -> MarcRecordRaw -> Maybe FieldMetadata
lookupFieldMetadata aTag record = if null results
                                  then Nothing
                                  else Just (head results)
    where metadata = (getFieldMetadata . splitDirectory . getDirectory) record
          results = filter ((== aTag) . tag) metadata

lookupSubfield :: Maybe FieldMetadata -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubfield Nothing _ _ = Nothing
lookupSubfield (Just metadata) subfield record =
    if null results
    then Nothing 
    else Just ((T.drop 1 . head) results)
    where rawField = getTextField record metadata
          subfields = T.split (==fieldDelimiter) rawField
          results = filter ((==subfield) . T.head) subfields

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subfield record = lookupSubfield fieldMetadata subfield record
    where fieldMetadata = lookupFieldMetadata aTag record

lookupTitle :: MarcRecordRaw -> Maybe T.Text
lookupTitle = lookupValue titleTag titleSubfield

lookupAuthor :: MarcRecordRaw -> Maybe T.Text
lookupAuthor = lookupValue authorTag authorSubfield

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream = zip titles authors
    where records = allRecords marcStream
          titles = map lookupTitle records
          authors = map lookupAuthor records

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs = map (\(title, author) -> Book {
                                                title = fromJust title
                                                , author = fromJust author
                                              }) justPairs
    where justPairs = filter (\(title, author) -> isJust title && isJust author) pairs

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . take n . marcToPairs

main :: IO ()
main = do
    marcData <- B.readFile "./samples/ohsu_ncnm_wscc_bibs.mrc"
    quantity <- getLine
    let html = processRecords (read quantity) marcData
    TIO.writeFile "./html/books.html" html