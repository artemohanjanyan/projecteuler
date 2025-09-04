import P007 (primes)

main = print $ sum $ takeWhile (<=2000000) primes
