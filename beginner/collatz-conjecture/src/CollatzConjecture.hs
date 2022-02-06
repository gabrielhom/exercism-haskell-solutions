module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | n == 1 = Just 0
  | even n = collatz (div n 2) >>= \x -> Just (x + 1)
  | otherwise = collatz (3 * n + 1) >>= \x -> Just (x + 1)
