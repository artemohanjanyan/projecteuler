module P030 (digits) where

digits :: Int -> [Int]
digits n = map (read . (:[])) $ show n

check n = n == sum (map (^5) $ digits n)

main = print $ sum $ filter check [2..1000000]
