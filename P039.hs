solve p = [(a, b, c)
  | a <- [1 .. p - 2]
  , b <- [1 .. p - a - 1]
  , let c = p - a - b
  , a < b && b < c
  , a ^ 2 + b ^ 2 == c ^ 2
  ]

main = print $ snd $ maximum $ map (\n -> (length (solve n), n)) [1..1000]
