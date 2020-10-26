pythagoreanTriple :: Int -> Int -> Int -> Bool
pythagoreanTriple a b c
  | a ^ 2 + b ^ 2 == c ^ 2 = True
  | a ^ 2 + c ^ 2 == b ^ 2 = True
  | b ^ 2 + c ^ 2 == a ^ 2 = True
  | otherwise = False

