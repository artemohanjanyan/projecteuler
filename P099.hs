import Data.Maybe (fromJust)
import Data.List (findIndex, sort)

parseLine :: String -> (Integer, Integer)
parseLine str =
  let
    commaIndex = fromJust (findIndex (== ',') str)
    (a, _:b) = splitAt commaIndex str
  in (read a, read b)

main = do
  numbers <- (flip zip [1..] . map (\(a, b) -> a ^ b). map parseLine . lines) <$> getContents
  print $ snd $ last $ sort numbers
