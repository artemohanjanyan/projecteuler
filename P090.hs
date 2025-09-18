import P030 (digits)

chain 1 = 1
chain 89 = 89
chain n = chain $ sum $ map (^2) $ digits n

main = print $ length $ filter (==89) $ map chain [1..9999999]
