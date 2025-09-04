import P002 (fibs)

main = print $ snd $ head $ filter (\(n, _) -> length (show n) >= 1000) $ zip fibs [0..]
