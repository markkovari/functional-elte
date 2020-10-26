mountain :: Int -> [Int]
mountain a
  | a <= 0 = []
  | a == 1 = [1]
  | a == 2 = [1, 2, 1]
  | otherwise = [1, 2 .. a] ++ [(a -1), (a -2) .. 1]

