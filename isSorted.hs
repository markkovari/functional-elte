--((4 + ((5 * ((3 ^ 6) ^ 3)) `mod` 2)) - 1)

isSorted :: Ord a => (a, a, a) -> Bool
isSorted (a, b, c) = a <= b && b <= c