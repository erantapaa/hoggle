{-# LANGUAGE OverloadedStrings #-}

module Run
where

import Solver
import Lib
import Grid
import System.Environment
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Data.Char
import Data.List
import Data.List.Ordered (nubSort)
import qualified Data.Trie as T
import System.TimeIt
import Text.Printf

usage = do
  putStrLn $ unlines
    [ "Usage: PROGRAM trie-impl dictionary-path board"
    , ""
    , "Trie Implementation:"
    , "  bs            - binary search on a ByteString"
    , "  trie          - Data.Trie created with foldl'"
    , "  trie-fromlist - Data.Trie created with fromList"
    , ""
    , "Boards:"
    , ""
    , "  best4         - best 4x4 board"
    , "  best5         - best 5x5 board"
    , "  best6         - best 6x6 board"
    , "  ...           - the actual board letters (16, 25 or 36)"
    ]

knownTrieImpls = [ "bs", "trie", "trie-fromlist" ]

main = do
  args <- getArgs
  if not ((length args == 3) && ( elem (map toLower (args !! 0)) knownTrieImpls ))
    then usage
    else main' args

main' (arg1 : dictPath : boardArg : _) = do
  let trieImpl = map toLower arg1

      letters = filter isAlpha $ 
                  case boardArg of
                    "best4" -> best4x4
                    "best5" -> best5x5
                    "best6" -> best6x6
                    _       -> boardArg
      (gw, gh, minlen)
        | len == 16 = (4, 4, 3)
        | len == 25 = (5, 5, 3)
        | len == 36 = (6, 6, 3)
        | otherwise = error "board not 16, 25 or 36 characters"
        where len = length letters

  db <- BS.readFile dictPath

  -- convert letters to match the case of the dictionary

  let bs | isLower ch = BS.pack $ map toLower letters
         | otherwise  = BS.pack $ map toUpper letters
         where 
           Just ch = BS.find isAlpha db

  (secs, (count, found)) <- timeItT $ do
    let lookup = case trieImpl of
                  "bs"            -> initLib minlen db
                  "trie"          -> initTrie minlen db
                  "trie-fromlist" -> initTrieFromList minlen db
                  _               -> error ("unknown trie implementation: " ++ trieImpl)

        grid = Grid bs gw gh
        found = nubSort $ solve grid lookup
        count = length found

    putStrLn $ "Words found: " ++ show count
    return (count, found)

  -- forM_ found BS.putStrLn
  putStrLn $ printf "time: %.3f secs - words: %d" secs count

-- return a lookup function using Data.Trie
-- the lookup function returns booleans: (is-prefix, is-word)
initTrie minlen db = 
  let t = foldl' go T.empty (BS.words db)
         -- this is slower: T.fromList [ (w,()) | w <- BS.words db ]
        where go t w = T.insert w () t
      lookup w = T.lookupBy go w t
        where go (Just _) t' = (True, BS.length w >= minlen)
              go Nothing  t' = (not (T.null t'), False)
  in
  lookup

initTrieFromList minlen db = 
  let t = T.fromList [ (w,()) | w <- BS.words db ]
        where go t w = T.insert w () t
      lookup w = T.lookupBy go w t
        where go (Just _) t' = (True, BS.length w >= minlen)
              go Nothing  t' = (not (T.null t'), False)
  in
  lookup

-- return a lookup function using Lib
initLib minlen db =
  let lookup w
        | p >= BS.length db = (False, False)
        | otherwise         = (BS.isPrefixOf w w', BS.length w >= minlen && w == w')
        where p = findLT db w
              w' = takeWord db p
  in lookup 

showWords dictPath board = do
  let letters = filter isAlpha board
      (gw, gh, minlen)
        | len == 16 = (4, 4, 3)
        | len == 25 = (5, 5, 3)
        | len == 36 = (6, 6, 3)
        | otherwise = error "board not 16, 25 or 36 characters"
        where len = length letters
  db <- BS.readFile dictPath

  -- convert the board to match the case of the word list
  let bs | isLower ch = BS.pack $ map toLower letters
         | otherwise  = BS.pack $ map toUpper letters
         where 
           Just ch = BS.find isAlpha db
  
  -- the lookup function:
  let lookup = initTrie minlen db  -- alternatively: initLib minlen db

      grid = Grid bs gw gh
      found = nubSort $ solve grid lookup
      count = length found

  forM_ found BS.putStrLn
  -- BS.writeFile "output" $ BS.unlines found
  putStrLn $ "Words found: " ++ show count

-- The densest 4x4 board: 1414 words
best4x4 = "S E R S P A T G L I N E S E R S"

-- The densest 5x5 board: 3120+ words
best5x5 = " R S C L S D E I A E G N T R P I A E S O L M I D C"

-- The densest 6x6 board: 5243 words (with dictionary 6x6.txt)
best6x6 = " D S R O D G T E M E N S R A S I T O D G N T R P R E I A E S T S C L P D"

