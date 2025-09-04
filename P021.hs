import P003 (factors)

d n = 1 + sum (factors n) - n

isAmicable n = n == d (d n) && n /= d n

main = print $ sum $ filter isAmicable [2..10000]
