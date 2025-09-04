power a 1 = a
power a n = a * power a (n - 1) `mod` 10000000000

main = print (sum (map (\n -> power n n) [1..1000]) `mod` 10000000000)
