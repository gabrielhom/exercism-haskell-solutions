module BinarySearch (find) where

import Data.Array

find :: Ord a => Array Int a -> a -> Maybe Int
find array x = binarySearch (bounds array)
  where
    binarySearch (lo, hi)
      | lo > hi = Nothing
      | array ! mid < x = binarySearch (lo + 1, hi)
      | array ! mid > x = binarySearch (lo, hi - 1)
      | otherwise = Just mid
      where
        mid = (lo + hi) `div` 2
