solve :: [[Int]] -> [Int]
solve [] = []
solve [xs] = xs
solve (prev : next : after) = solve $ combine (0:prev) next : after

combine :: [Int] -> [Int] -> [Int]
combine [] [] = []
combine (n:a:as) (b:bs) = (max n a + b) : combine (a:as) bs
combine [a] [b] = [a + b]

main = do
  input <- map (map read) . map words . lines <$> getContents :: IO [[Int]]
  print $ maximum $ solve input
