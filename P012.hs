import P003 (factorization)

import Data.List (group)

triangle n = n * (n + 1) `div` 2

triangles = map triangle [1..]

numberOfDivisors n = foldr (\a b -> (a + 1) * b) 1 $ map length $ group $ factorization n

main = print $ head $ filter (\t -> numberOfDivisors t >= 500) triangles
