
module Grid
  ( Grid(..)
  , gridAt
  , gridInRange
  , gridNeighbors
  , gridCells
  , gridIndex
  )
where

import qualified Data.ByteString.Char8 as BS
import           Data.ByteString          (ByteString)
import qualified Data.Set              as Set

data Grid = Grid { gchars :: ByteString, gwidth :: Int, gheight :: Int }

gridAt :: Grid -> (Int, Int) -> Char
gridAt g (r,c) = BS.index (gchars g) offset
  where offset = r * (gwidth g) + c

gridInRange :: Grid -> (Int,Int) -> Bool
gridInRange g (r,c) = (0 <= r && r < gheight g) && (0 <= c && c < gwidth g)

gridNeighbors g (r,c) = 
  filter (gridInRange g) [ (r-1,c), (r,c-1), (r+1,c), (r,c+1), (r-1,c-1), (r-1,c+1), (r+1,c-1), (r+1,c+1) ]

gridCells g = [ (r,c) | r <- [0.. (gheight g)-1], c <- [0..(gwidth g)-1] ]

gridIndex g (r,c) = r * (gwidth g) + c

