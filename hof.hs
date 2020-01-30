import Data.List

ifEven :: (Int -> Int) -> Int -> Int
ifEven func x = if even x
           then func x
           else x

ifEvenInc = ifEven (+ 1)

ifEvenDouble = ifEven (* 2)

ifEvenSquare = ifEven (^ 2)

names = [("Ian", "McKelian"), ("Bernard","Sumner"),
            ("Peter", "Straub"), ("Stephen","King"), ("Joe", "King")]


compareLastNames (firstNameA, lastNameA) (firstNameB, lastNameB)
    | lastNameA > lastNameB = GT
    | lastNameA < lastNameB = LT
    | firstNameA > firstNameB = GT
    | firstNameA < firstNameB = LT
    | otherwise = EQ

sfOffice (firstName, lastName) = if lastName < "L"
                                    then nameText ++ " - PO Box 1234 - San Francisco, CA, 94111"
                                    else nameText ++ " - PO Box 1010 - San Francisco, CA, 94109"
                                where nameText = firstName ++ " " ++ lastName

getLocationFunction location = case location of
    "ny" -> \ (firstName, lastName) -> firstName ++ " " ++ lastName ++ ": PO Box 789 - New York, NY, 10013"
    "sf" -> sfOffice
    "reno" -> \ (_, lastName) -> lastName ++ " - PO Box 456 - Reno, NV 89523"
    _ -> \ (firstName, lastName) -> firstName ++ " " ++ lastName

addressLetter name location  = locationFunc name
    where locationFunc = getLocationFunction location

getRequestURL host apiKey resource userId = host ++ "/" ++ resource ++ "/" ++ userId ++ "?token=" ++ apiKey

genApiRequest = getRequestURL "http://example.com/" "1337hAsk3ll" "book"

flipBinaryArgs func = (\ x y -> func y x)

addressLetterV2 = flipBinaryArgs addressLetter

subtract2 = flip (-) 2

binaryPartialApplication f x = (\y -> f x y)
