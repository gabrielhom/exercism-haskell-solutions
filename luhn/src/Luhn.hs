module Luhn (isValid) where

import Data.Char

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

onlyDigits :: [Char] -> [Int]
onlyDigits n = map (\ch -> read [ch] :: Int) (filter isDigit n)
