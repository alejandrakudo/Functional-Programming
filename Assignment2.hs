module Assignment2 where

import OlympicDatabase

-- PART 1: LIST FUNCTIONS

-- doubles all lower-case vowels
doubleVowels :: String -> String
doubleVowels "" = ""
doubleVowels (c:tail)
    | elem c "aeiou" = c:[c] ++ (doubleVowels tail)
    | otherwise = c:(doubleVowels tail)

pairFlip :: [a] -> [a]
pairFlip [] = []
pairFlip [x] = [x]
pairFlip (x:y:tail) = y:x:(pairFlip tail)

-- returns a list of all of the sublists of a list which add up to a specified total
sublistSum [] 0 = [[]]
sublistSum [] _ = []
sublistSum (x:xs) sum = 
    (sublistSum xs sum) ++
    [x:sub | sub <- sublistSum xs (sum-x)]


-- PART 2: FUNCTIONS USING AN OLYMPIC DATABASE (LISTS & TUPLES)

-- Given the name of an event, a year and a medal list, returns the name of 
-- the runner up (2nd place winner) for that event and that year inside 
-- the medal list.  Assumes there will not be a duplicate.  If there is no fact
-- that tells you who the runner-up was for that event in that year, 
-- returns an empty string ("")
runnerUp :: String -> Integer -> ResultList -> String
runnerUp _ _ [] = ""
runnerUp event year ((ev,yr,medal,name,_country):moreResults)
    | year == yr && event == ev && medal == 2 = name 
    | otherwise = runnerUp event year moreResults
    
-- Given the name of an athlete and a medal list, returns a list of the
-- medals that athlete has earned.  If the athlete didn't earned any medals,
-- the list will be empty.  If the athlete earned at least one medal,
-- the elements of the list will be tuples of the form (event,year)
medals :: String -> ResultList -> [(String,Integer)]
medals _ [] = []
medals name ((event,year,medal,name2,_country):moreResults)
    | name == name2 = (event,year):medalsInTail
    | otherwise = medalsInTail
    where
    medalsInTail = medals name moreResults
    
-- Given the name of a country, a year, and a result list, returns the number
-- of gold medals the country won in that year
goldCount :: String -> Integer -> ResultList -> Integer
goldCount _  _ [] = 0
goldCount country year ((_,yr,color,_,nation):moreResults)
    | color == 1 && nation == country && yr == year = 1 + moreGolds
    | otherwise = moreGolds 
    where
    moreGolds = goldCount country year moreResults
    



