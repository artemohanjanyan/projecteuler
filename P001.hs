main = print $ sum $ takeWhile (< 1000) $ filter (\a -> a `mod` 3 == 0 || a `mod` 5 == 0) [1..]
