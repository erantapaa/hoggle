module Solver
where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import qualified Data.Set as Set

data Grid = Grid { gchars :: ByteString, gwidth :: Int, gheight :: Int }

gridAt :: Grid -> (Int, Int) -> Char
gridAt g (r,c) = BS.index (gchars g) offset
  where offset = r * (gwidth g) + c

gridInRange :: Grid -> (Int,Int) -> Bool
gridInRange g (r,c) = (0 <= r && r < gheight g) && (0 <= c && c < gwidth g)

gridNeighbors g (r,c) = 
  filter (gridInRange g) [ (r-1,c), (r,c-1), (r+1,c), (r,c+1), (r-1,c-1), (r-1,c+1), (r+1,c-1), (r+1,c+1) ]

gridCells g = [ (r,c) | r <- [0.. (gheight g)-1], c <- [0..(gwidth g)-1] ]

-- A Lookup function returns (isPrefix, isWord) for a word w
-- where isPrefix is True if w is the prefix of some dictionary word
-- and isWord is True if w is a word in the dictionary

type Lookup = ByteString -> (Bool, Bool)

solve :: Grid -> Lookup -> [ByteString]
solve g lookup = concatMap (go Set.empty BS.empty) (gridCells g)
  where
    go seen w rc
      | Set.member rc seen = []
      | notPrefix          = []
      | otherwise          = (if isWord then [w'] else [])
                                ++ concatMap (go seen' w') nbrs
      where 
        w' = BS.snoc w (gridAt g rc)
        (isPrefix, isWord) = lookup w'
        notPrefix = not isPrefix
        nbrs = gridNeighbors g rc
        seen' = Set.insert rc seen

