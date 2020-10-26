isLeapYear :: Int -> Bool
isLeapYear y = divisibleBy 400 || (divisibleBy 4 && not (divisibleBy 100))
  where
    divisibleBy x = mod y x == 0