module P004 (isPalindrome) where

isPalindrome :: Show a => a -> Bool
isPalindrome n = show n == reverse (show n)

products = [a * b | a <- [999, 998 .. 100], b <- [999, 998 .. 100]]

main = print $ maximum $ filter isPalindrome products
