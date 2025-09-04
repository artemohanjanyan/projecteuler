nextPermutation :: Ord a => [a] -> [a]
nextPermutation [] = []
nextPermutation [x] = [x]
nextPermutation list = go list []
  where
    go [] acc = acc
    go (x:xs) [] = go xs [x]
    go (x:xs) (a:acc)
      | a < x = go xs (x:a:acc)
      | otherwise = insert [] x (a:acc) ++ xs

    insert (a:acc) x [] = reverse (x:acc) ++ [a]
    insert (a:acc) x (r:rest) | x > r = (reverse acc ++ [x] ++ (r:rest)) ++ [a]
    insert acc x (r:rest) = insert (r:acc) x rest

main = putStrLn $ concat $ map show $ reverse $ (iterate nextPermutation [9, 8 .. 0] !! 999999)
