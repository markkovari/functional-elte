isLetter :: Char -> Bool
isLetter l
  | elem l (['a' .. 'z'] ++ ['A' .. 'Z']) = True
  | otherwise = False