respond x = if '!' `elem` x
            then "wow!"
            else "ohhhh... mkay..."

ones n = take n (cycle [1])

assignToGroups n aList = zip groups aList
    where groups = cycle [1..n]


repeat' x = cycle [x]

subseq start end list = drop start cropped
    where cropped = take end list

inFirstHalf element list = element `elem` half
    where half = take ((length list) `div` 2) list
