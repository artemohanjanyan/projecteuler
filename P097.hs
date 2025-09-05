mul :: Int -> Int -> Int
mul a b = a * b `mod` 10000000000

n = foldr mul 28433 (take 7830457 (repeat 2)) + 1

main = print n
