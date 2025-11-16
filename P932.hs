is2025 :: Int -> Bool
is2025 n | n < 10 = False
is2025 n =
  let
    nStr = show n
    nL = length nStr
    (p1, p2) = splitAt (nL `div` 2) nStr
    n1 = read p1 :: Int
    n2 = read p2 :: Int
  in (n1 + n2) ^ 2 == n && (show n1 ++ show n2 == nStr)

t :: Int -> Int
t n = sum $ filter is2025 $ map (^2) [1..(10^((n + 1) `div` 2) - 1)]

main :: IO ()
main = print (t 16)
