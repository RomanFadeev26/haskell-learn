cup f10z = \message -> message f10z

getOz aCup = aCup id

drink aCup ozDrank = if zDiffs >= 0
    then cup (f10z - ozDrank)
    else cup 0
    where f10z = getOz aCup
          zDiffs = f10z - ozDrank