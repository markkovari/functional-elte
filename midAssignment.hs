data Sign = Rock | Paper | Scissors deriving (Eq, Show)

type Player = (String, [Sign])

data Result = Player1 | Player2 | Draw deriving (Eq, Show)

-- can be useful to use
-- abs, cycle, filter, foldl, foldr, map, repeat, sum, take, zip, zipWith

anna :: Player
john :: Player
george :: Player
anna = ("Anna", cycle [Rock, Paper, Scissors])

john = ("John", cycle [Paper])

georgeStrategy :: [Int] -> [Sign]
georgeStrategy (round : remaining)
  | mod round 7 == 5 = [Rock] ++ georgeStrategy remaining
  | mod round 3 == 0 = [Paper] ++ georgeStrategy remaining
  | otherwise = [Scissors] ++ georgeStrategy remaining

george = ("George", georgeStrategy [1 ..])

convert :: Sign -> Sign
convert Rock = Paper
convert Paper = Scissors
convert Scissors = Rock

signListConverter :: [Sign] -> [Sign]
signListConverter [] = []
signListConverter (x : xs) = convert x : signListConverter xs

compareSign :: Sign -> Sign -> Result
compareSign f s
  | convert f == s = Player2
  | convert s == f = Player1
  | otherwise = Draw

compareSigns :: [Sign] -> [Sign] -> [Result]
compareSigns [] [] = []
compareSigns _ [] = []
compareSigns [] _ = []
compareSigns (x : xs) (y : ys) = compareSign x y : compareSigns xs ys

fightPlayers :: Player -> Player -> [Result]
fightPlayers p1 p2 = compareSigns (snd p1) (snd p2)

-- maybe an outcome map can be still appropriate here, something like
-- let pointAssociation = [(Player1 ,1), (Player2, (-1)), (Draw, 0)]

evaluateResult :: Result -> Int
evaluateResult result
  | result == Player1 = 1
  | result == Draw = 0
  | otherwise = -1

calculatePoints :: [Result] -> Int
calculatePoints [] = 0
calculatePoints (p : rest) = evaluateResult p + calculatePoints rest

fightPlayersUntil :: Player -> Player -> Int -> [Result]
fightPlayersUntil p1 p2 r = take r (fightPlayers p1 p2)

tournament :: Player -> Player -> Int -> (String, Int)
tournament p1 p2 rounds
  | result > 0 = (fst p1, abs result)
  | result < 0 = (fst p2, abs result)
  | otherwise = (fst p1 ++ "/" ++ fst p2, 0)
  where
    result = calculatePoints (fightPlayersUntil p1 p2 rounds)
