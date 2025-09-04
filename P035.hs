import P007 (primes)
import P030 (digits)
import Data.List (inits, tails)
import Data.IntSet (fromList, member)

intPrimes = takeWhile (<1000000) primes :: [Int]

primeSet = fromList intPrimes

isCircular n =
  let
    str = show n
    nInits = tail $ inits str
    nTails = tail $ tails str
  in all (`member` primeSet) (map read (zipWith (++) nTails nInits))

main = print $ length $ filter isCircular intPrimes
