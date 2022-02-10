module Phone (number) where

import Data.Char (digitToInt, isDigit)

number :: String -> Maybe String
number = cleanNumber . filter isDigit
  where
    cleanNumber num = case length num of
      11 -> if head num == '1' then validate (tail num) else Nothing
      10 -> validate num
      _ -> Nothing

validate :: String -> Maybe String
validate xs
  | digitToInt (head xs) > 1 && digitToInt (xs !! 3) > 1 = Just xs
  | otherwise = Nothing
