{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Lib
where

import qualified Data.ByteString.Char8 as BS
import Data.ByteString (ByteString)
import Data.Monoid
import Debug.Trace

findLT :: ByteString -> ByteString -> Int
findLT db w = lowerbound db w 0 top
  where top = BS.length db

-- lb points to a word
-- return first word in [lb, ub) which is >= w
--
-- Uncomment the `tr $` in the definition of w' to
-- trace execution of the binary search.
lowerbound db w lb ub
  | mid <= lb    = linsearch  db w lb ub
  | w'end >= ub  = linsearch  db w lb ub
  | w' < w       = lowerbound db w w'end ub
  | otherwise    = lowerbound db w lb w'end
  where
    mid   = backup db $ quot (lb + ub) 2
    w'    = {- tr $ -} takeWord db mid
    w'end = mid + BS.length w' + 1
    tr x  = trace msg x
      where msg = "lb: " ++ show (lb, lbw) ++ " mid: " ++ show (mid, x) ++ " ub: " ++ show (ub, ubw)
            lbw = takeWord db lb
            ubw = takeWord db ub

-- perform a linear search for w in the range [lb, ub)
linsearch db w lb ub
  | lb >= ub  = ub
  | w' >= w   = lb
  | otherwise = linsearch db w (lb + BS.length w' + 1) ub
  where w' = takeWord db lb

-- backup p to the beginning of a word
backup db p
  | p <= 0                    = 0
  | BS.index db (p-1) == '\n' = p
  | otherwise                 = backup db (p-1)

-- advance p to the next word
advance db top p
  | p >= top              = top
  | BS.index db p == '\n' = p+1
  | otherwise             = advance db top (p+1)

-- extract the word at position p
-- assume p < length of db
takeWord db p = BS.takeWhile (/= '\n') $ BS.drop p db

test db w = do
  let p = findLT db w
      w' = takeWord db p
  BS.putStrLn $ "lowerbound " <> w <> " -> " <> w' <> " at position: " <> BS.pack (show p)

