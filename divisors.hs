divisors :: Int -> [Int]
divisors a
  | a == 0 = [1 ..]
  | a == 1 = [1]
  | otherwise = filter (\n -> a `mod` n == 0) [1 .. a]

