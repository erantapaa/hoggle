module Run
where

import Solver
import Lib
import System.Environment
import qualified Data.ByteString.Char8 as BS
import Control.Monad
import Data.Char
import Data.List
import Data.List.Ordered (nubSort)

main = do
  (dictPath : board : _) <- getArgs
  showWords dictPath board

showWords dictPath board = do
  let letters = filter isAlpha board
      (gw, gh, minlen)
        | len == 16 = (4, 4, 3)
        | len == 25 = (5, 5, 3)
        | otherwise          = error "board not 16 or 25 characters"
        where len = length letters
  db <- BS.readFile dictPath

  -- convert the board to match the case of the word list
  let bs | isLower ch = BS.pack $ map toLower letters
         | otherwise  = BS.pack $ map toUpper letters
         where 
           Just ch = BS.find isAlpha db
  
  -- the lookup function:
  let lookup w
        | p >= BS.length db = (False, False)
        | otherwise         = (BS.isPrefixOf w w', BS.length w >= minlen && w == w')
        where p = findLT db w
              w' = takeWord db p

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

