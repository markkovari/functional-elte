pow :: Int -> Int -> Int
pow base exponent
  | base == 0 = 0
  | base == 1 = 1
  | exponent == 1 = base
  | exponent == 0 = 1
  | otherwise = base * pow base (exponent -1)
