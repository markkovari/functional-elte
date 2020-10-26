distantPair :: (Integer, Integer) -> Bool
distantPair (a, b)
  | 2 <= abs (a - b) = True
  | otherwise = False

distantPairs :: [(Integer, Integer)] -> Int
distantPairs x
  | x == [] = 0
distantPairs (x : xs)
  | xs == [] = if distantPair x then 1 else 0
  | distantPair x == True = 1 + distantPairs xs
  | otherwise = 0 + distantPairs xs