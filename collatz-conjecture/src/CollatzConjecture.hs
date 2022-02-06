module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
    | n <= 0 = Nothing
    | otherwise = Just $ fromIntegral (length (collatzSeq n) - 1)

collatzSeq :: Integer -> [Integer]
collatzSeq 1 = [1]
collatzSeq n
    | even n =  n : collatzSeq (n `div` 2)
    | odd n  =  n : collatzSeq (n * 3 + 1)
    | otherwise = error "Not a natural number"