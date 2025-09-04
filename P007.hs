{-# LANGUAGE ScopedTypeVariables #-}

module P007 (sqrtI, primes, main) where

sqrtI :: Integral a => a -> a
sqrtI n = go 1 n
  where
    go l r
      | l + 1 == r = l
      | otherwise =
        let mid = l + (r - l) `div` 2
        in if mid * mid > n
          then go l mid
          else go mid r

primes :: forall a. Integral a => [a]
primes = ans
  where
    ans = 2 : filter isPrime [3..]

    isPrime :: a -> Bool
    isPrime n = not (any (\m -> n `mod` m == 0) $ takeWhile (<= sqrtI n) ans)

main = print $ (primes !! 10000 :: Int)
