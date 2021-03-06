module Luhn (isValid) where

import Data.Char (isDigit)

isValid :: String -> Bool
isValid n
  | length (onlyDigits n) <= 1 = False
  | otherwise =
    sum
      ( zipWith
          ($)
          (cycle [(*) 1, \x -> if x < 5 then 2 * x else 2 * x - 9])
          (reverse (onlyDigits n))
      )
      `mod` 10 == 0
  where
    onlyDigits = map (\ch -> read [ch] :: Int) . filter isDigit
