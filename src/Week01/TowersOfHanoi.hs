module Week01.TowersOfHanoi
  ( hanoi
  )
where

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = moves1 ++ [(a,b)] ++ moves3
  where
    moves1 = hanoi (n - 1) a c b
    moves3 = hanoi (n - 1) c b a
