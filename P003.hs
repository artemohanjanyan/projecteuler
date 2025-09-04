module P003 (factorization, factors) where

import P007 (primes)

n = 600851475143

factors n = filter (\a -> n `mod` a == 0) [2..n]

factorization n = go n
  where
    go 1 = []
    go m =                                     -- or    (factors n)
      let factor = head $ filter (\a -> m `mod` a == 0) primes
      in factor : go (m `div` factor)

main = print $ maximum $ factorization n
