calc :: (Int, Char, Int) -> Int
calc (o1, op, o2)
  | op == '+' = o1 + o2
  | op == '-' = o1 - o2
  | op == '*' = o1 * o2
  | op == '/' = div o1 o2
