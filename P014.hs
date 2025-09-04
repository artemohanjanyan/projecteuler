import Data.List (maximumBy)
import Data.Function (on)

collatz 1 = 1
collatz n
  | n `mod` 2 == 0 = 1 + collatz (n `div` 2)
  | otherwise      = 1 + collatz (3 * n + 1)

main = print $ fst $ maximumBy (compare `on` snd) $ map (\n -> (n, collatz n)) [1..999999]
