-- Solution for Assignment 3
-- CISC 260, Winter 2018
module Assn3Solution where

data Trie = MakeTrie Char [Trie]
    deriving Eq
    
-- showTrie creates a multi-line string describing a trie in 
-- indented form (very much like the show function for 
-- indented representations of family trees the family trees we 
-- discussed in class)
-- There will be an extra newline at the beginning of the result
showTrie :: Int -> Trie -> String
showTrie level (MakeTrie c []) = "\n" ++ (indent level) ++ [c]
showTrie level (MakeTrie c [subTrie]) = "\n" ++ (indent level) 
    ++ c:(showTrie (level+1) subTrie)
showTrie level (MakeTrie c subTrieList) = "\n" ++ (indent level) 
    ++ c:concat (map (showTrie (level+1)) subTrieList)
    
-- Helper function: indent n returns a string with 2*n spaces in it.
indent :: Int -> String
indent n
    | n <= 0 = ""
    | otherwise = "  " ++ (indent (n-1))

-- Specify how tries will be displayed by the interpreter
instance Show Trie 
    where show t = tail (showTrie 0 t) -- getting rid of extra newline at beginning
    

-- A simple trie containing "cat", "can" and "dog"
simpleTrie = 
    MakeTrie '.' [
        MakeTrie 'c' [
            MakeTrie 'a' [
                lastLetter 'n',
                lastLetter 't'
                ]
            ],
        MakeTrie 'd' [
                MakeTrie 'o' [
                    lastLetter 'g'
                ]
            ]
        ]

-- helper to make the above shorter:
-- a trie containing a single character at the end of a word 
lastLetter ch = MakeTrie ch [MakeTrie '$' []]

-- Searches for a word in a complete trie and returns True if it -- finds the word.  The word parameter should include lower-case -- letters only -- no starting '.' or ending '$'.
searchWord :: String -> Trie -> Bool
searchWord word trie = searchString ('.':word++"$") trie

-- Searches for a string in a trie.  The trie may be a complete 
-- trie with '.' at the root or an inner tree with a letter as the 
-- root.  The string should include all characters to be searched 
-- for, including a '$' at the end.
searchString :: String -> Trie -> Bool
searchString "" _ = False
searchString (c:cs) (MakeTrie letter subTries)
    | c == letter = searchTrieList cs subTries
    | otherwise = False

-- Searches for a string in a list of tries.
searchTrieList "" _ = True
searchTrieList _ [] = False
searchTrieList word (trie:moreTries) =
    searchString word trie || searchTrieList word moreTries
    
-- Returns an alphabetical list of all of the words in a 
-- complete trie.
wordsInTrie :: Trie -> [String]
wordsInTrie trie = map strip (stringsInTrie trie)
    where
    -- strip removes the starting '.' and ending '$' from a string
    -- No checking; assumes the string starts with '.' and ends 
    -- with '$'
    strip (c:cs) = init cs

-- Returns an alphabetical list of all the strings in a trie or 
-- sub-trie, including '.' and '$' characters.
stringsInTrie :: Trie -> [String]
stringsInTrie (MakeTrie letter []) = [[letter]]
stringsInTrie (MakeTrie letter subTries) = 
    map (letter :) (concat (map stringsInTrie subTries))

-- Adds a word to a complete trie (i.e. a trie starting with 
-- '.').  The word does not include the starting "." or ending "$"
addWordToTrie :: String -> Trie -> Trie
addWordToTrie word (MakeTrie '.' children) =
    (MakeTrie '.' (addStringToTrieList (word ++ "$") children))

-- Adds a string to a list of tries.  All or part of the string 
-- may already be in one of the tries
addStringToTrieList :: String -> [Trie] -> [Trie]
-- adding an empty string to a list of tries has no effect
addStringToTrieList "" list = list
-- If the list of tries is empty, create another trie to hold the 
-- string
addStringToTrieList (c:cs) [] = 
    [MakeTrie c (addStringToTrieList cs [])]
addStringToTrieList (c:cs) ((MakeTrie letter subTries):moreTries) 
    -- Case 1: the first trie in the list starts with the first 
    -- letter of the string.  Add the tail of the string to this 
    -- first trie and leave the remaining tries unchanged.
    | c == letter = 
        (MakeTrie letter 
            (addStringToTrieList cs subTries)):moreTries
    -- Case 2: the string belongs in a new trie alphabetized
    -- before the first trie in the list.  So create a new 
    -- trie to hold the string and be the first in the new 
    -- list of tries
    | c < letter = 
        (MakeTrie c (addStringToTrieList cs [])):
            (MakeTrie letter subTries):moreTries
    -- Case 3: The string goes after the first trie in the list, 
    -- so add it to the tail of the list.
    | otherwise = (MakeTrie letter subTries) : 
        (addStringToTrieList (c:cs) moreTries)
    
-- Creates a trie holding all of the words in a list of words
createTrie :: [String] -> Trie
createTrie wordList = 
    foldr addWordToTrie (MakeTrie '.' []) wordList  

-- The trie from the picture on the assignment page.  
-- (Note that "cat" is added twice and 
-- will not result in duplication in the trie.)
webTrie = createTrie ["cat","dog","cab","dime","dim","candy",
                      "cat","catnip","catalog","do"]

-- Example from requirements document
bigTrie = createTrie ["queen","king","quite","kind","quantum","kill","quantity","kite","quality","keep"]


    
    

