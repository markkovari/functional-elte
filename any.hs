any' :: (a -> Bool) -> [a] -> Bool
any' _ [] = False
any' pred (x : xs)
  | pred x = True
  | otherwise = any' pred xs
