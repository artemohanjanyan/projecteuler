import P003 (factorization)

import Data.List (foldl')
import qualified Data.IntMap.Strict as M

newtype Factorized = Factorized [(Int, Int)]
  deriving (Show, Eq)

instance Enum Factorized where
  toEnum n = Factorized $ group $ factorization n
    where
      group [] = []
      group xs@(x:_) =
        let (xs', xs'') = span (== x) xs
        in (x, length xs') : group xs''

  fromEnum (Factorized xs) = product $ map (\(a, b) -> a ^ b) xs

instance Num Factorized where
  a + b = toEnum $ (fromEnum a :: Int) + fromEnum b

  a * (Factorized []) = a
  (Factorized []) * b = b
  a'@(Factorized ((a, an) : as)) * b'@(Factorized ((b, bn) : bs))
    | a < b =
      let (Factorized rec) = Factorized as * b'
      in Factorized $ (a, an) : rec
    | a > b = b' * a'
    | a == b =
      let (Factorized rec) = Factorized as * Factorized bs
      in Factorized $ (a, an + bn) : rec

  fromInteger n = toEnum $ fromIntegral n

  signum = const (Factorized [])
  abs = undefined
  negate = undefined

efficientProducts :: Num a => [a] -> [a]
efficientProducts [] = undefined
efficientProducts (x:xs) = reverse $ foldr (\n (y:ys) -> (n * y) : y : ys) [x] $ reverse xs

factorials :: (Num a, Enum a) => a -> [a]
factorials n = efficientProducts [1..n]

superfactorials :: (Num a, Enum a) => a -> [a]
superfactorials n = efficientProducts $ factorials n

superpuperfactorials :: (Num a, Enum a) => a -> [a]
superpuperfactorials n = efficientProducts $ superfactorials n

modulo' :: Int
modulo' = 999999001

modulo :: Int -> Int
modulo n = n `mod` modulo'

infixl 6 +++
(+++) :: Int -> Int -> Int
a +++ b = modulo (a + b)

infixl 6 +--
(+--) :: Int -> Int -> Int
a +-- b = modulo (a - b + modulo')

infixl 7 ***
(***) :: Int -> Int -> Int
a *** b = modulo (a * b)

infixr 8 ^^^
(^^^) :: Int -> Int -> Int
a ^^^ n | n < 0 = error "we don't do that here"
a ^^^ 0 = 1
a ^^^ n
  | n `mod` 2 == 0 = let rec = a ^^^ (n `div` 2) in rec *** rec
  | otherwise = a *** a ^^^ (n - 1)

infixl 7 ///
(///) :: Int -> Int -> Int
a /// b = a *** b ^^^ (modulo' - 2)

sum' :: [Int] -> Int
sum' = foldl' (+++) 0

-- a ^ rem + a ^ (rem + 1000) + a ^ (rem + 2000) + ...
--   where   rem + n * 1000 <= maxN
powSum :: Int -> Int -> Int -> Int
powSum a rem' maxN =
  let r = a ^^^ 1000
      n = (maxN - rem') `div` 1000 + 1
  in a ^^^ rem' *** (r ^^^ n +-- 1) /// (r +-- 1)

supdupfact1000 :: [(Int, Int)]
supdupfact1000 =
  let Factorized xs = last $ superpuperfactorials 1000
  in xs

dp0 = M.fromList $ (0, 1) : [(n, 0) | n <- [1..999]]

step'' dp (a, n) rem' prevRem =
  let currentRem = (rem' - prevRem + 1000) `mod` 1000
      prevDp = dp M.! prevRem
  in prevDp *** powSum a currentRem n

step' dp (a, n) rem' = sum' $ map (step'' dp (a, n) rem') [0..999]

step dp (a, n) =
  M.fromList $ zip [0..] $ map (step' dp (a, n)) [0..999]

ans = foldl' step dp0 supdupfact1000 M.! 0

main :: IO ()
main = print ans
