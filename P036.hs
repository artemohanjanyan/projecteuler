import P004 (isPalindrome)

base2 :: Int -> String
base2 0 = "0"
base2 1 = "1"
base2 n = base2 (n `div` 2) ++ (if n `mod` 2 == 0 then "0" else "1")

isDoublePalindrome n = isPalindrome n && isPalindrome (base2 n)

main = print $ sum $ filter isDoublePalindrome [1..999999]
