module P016 (sumOfDigits) where

sumOfDigits n = sum $ map (\n -> read (n:[])) $ show n

main = print $ sumOfDigits $ 2 ^ 1000
