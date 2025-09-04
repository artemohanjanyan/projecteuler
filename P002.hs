module P002 (fibs) where

fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

main = print $ sum $ takeWhile (< 4000000) $ filter (\a -> a `mod` 2 == 0) fibs
