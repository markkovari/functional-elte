toLowerChar :: Char -> Char
toLowerChar x = snd (filter (\(l, _) -> l == x) (zip ['A' .. 'Z'] ['a' .. 'z']) !! 0)

toLower :: Char -> Char
toLower x
  | elem x ['A' .. 'Z'] = toLowerChar x
  | otherwise = x

upperToLower :: [Char] -> [Char]
upperToLower "" = ""
upperToLower (x : xs)
  | elem x ['A' .. 'Z'] = [toLower x] ++ upperToLower xs
  | otherwise = upperToLower xs