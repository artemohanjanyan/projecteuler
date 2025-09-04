import P007 (sqrtI)

isTriplet :: Int -> Int -> Bool
isTriplet a b =
  let c = a * a + b * b
  in sqrtI c ^ 2 == c

triplets = [(a, b) | a <- [1..1000], b <- [1..1000], isTriplet a b]

(tripletA, tripletB) = head $ filter (\(a, b) -> sqrtI (a * a + b * b) + a + b == 1000) triplets

main = print $ tripletA * tripletB * sqrtI (tripletA * tripletA + tripletB * tripletB)
