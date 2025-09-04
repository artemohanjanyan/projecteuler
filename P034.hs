import P030 (digits)

check n = n == sum (map (\n -> product [1..n]) $ digits n)

main = print $ sum $ filter check [3..1000000]
