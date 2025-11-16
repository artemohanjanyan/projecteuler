digitN :: Int -> Int
digitN n | n < 10 = 1
digitN n = 1 + digitN (n `div` 10)

is2025 :: Int -> Bool
is2025 n | n < 10 = False
is2025 n =
  let
    nL = digitN n
    f = 10 ^ ((nL + 1) `div` 2)
    n1 = n `div` f
    n2 = n `mod` f
    n1L = digitN n1
    n2L = digitN n2
  in (n1 + n2) ^ 2 == n && (n1L + n2L == nL)

t :: Int -> Int
t n = sum $ filter is2025 $ map (^2) [1..(10^((n + 1) `div` 2) - 1)]

main :: IO ()
main = print (t 16)
