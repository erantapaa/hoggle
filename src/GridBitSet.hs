module GridBitSet
  (GridBitSet, empty, member, insert)
where

import Data.Bits
import Data.Int
import Grid      (Grid,gridIndex)

newtype GridBitSet = GridBitSet { unGridBitSet :: Int64 }

empty :: GridBitSet
empty = GridBitSet 0

member :: Grid -> (Int,Int) -> GridBitSet -> Bool
member g rc gbits = testBit (unGridBitSet gbits) (gridIndex g rc)

insert :: Grid -> (Int,Int) -> GridBitSet -> GridBitSet
insert g rc gbits = GridBitSet $ setBit (unGridBitSet gbits) (gridIndex g rc)

