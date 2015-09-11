module Solver
  ( Lookup
  , solve
  )
where

import           Grid
import qualified GridBitSet as Set

import qualified Data.ByteString.Char8 as BS
import           Data.ByteString (ByteString)

-- A Lookup function returns (isPrefix, isWord) for a word w
-- where isPrefix is True if w is the prefix of some dictionary word
-- and isWord is True if w is a word in the dictionary

type Lookup = ByteString -> (Bool, Bool)

solve :: Grid -> Lookup -> [ByteString]
solve g lookup = concatMap (go Set.empty BS.empty) (gridCells g)
  where
    go :: Set.GridBitSet -> ByteString -> (Int,Int) -> [ByteString]
    go seen w rc
      | Set.member g rc seen = []
      | notPrefix            = []
      | otherwise            = (if isWord then [w'] else [])
                                  ++ concatMap (go seen' w') nbrs
      where 
        w' = BS.snoc w (gridAt g rc)
        (isPrefix, isWord) = lookup w'
        notPrefix = not isPrefix
        nbrs = gridNeighbors g rc
        seen' = Set.insert g rc seen

