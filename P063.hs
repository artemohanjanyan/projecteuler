ansForBase base =
  length $
  takeWhile (\(power, n) -> power == length (show n)) $
  map (\power -> (power, base ^ power)) [1..]

main = print $ sum $ map ansForBase [1..9]
