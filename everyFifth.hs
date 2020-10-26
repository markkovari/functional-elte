everyFifth :: [a] -> [a]
everyFifth rem = map (\(_, y) -> y) (filter (\(x, _) -> mod x 5 == 0) (zip [0 .. (length rem)] rem))