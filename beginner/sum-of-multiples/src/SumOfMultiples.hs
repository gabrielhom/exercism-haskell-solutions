module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum (nub (concat ([[x, (x * 2) .. limit - 1] | x <- filter (> 0) factors])))
