hasLongLines :: [Char] -> Bool
hasLongLines line = any (\word -> length (words word) >= 3) (lines line)