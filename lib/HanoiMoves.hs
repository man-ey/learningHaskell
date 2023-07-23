type Peg = String
type Move = (Peg, Peg)
hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi x from over to
    | x == 1 = [(from,to)]
    | otherwise = hanoi (x - 1) from to over ++ [(from, to)] ++ hanoi (x - 1) over from to