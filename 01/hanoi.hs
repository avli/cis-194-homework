{-# OPTIONS_GHC -Wall #-}

module Hanoi where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi numberOfDisks fromPeg toPeg tempPeg
  | numberOfDisks == 1 = [(fromPeg, toPeg)]
  | numberOfDisks == 2 = [(fromPeg, tempPeg), (fromPeg, toPeg), (tempPeg, toPeg)]
  | otherwise = hanoi (numberOfDisks - 1) fromPeg tempPeg toPeg
    ++ [(fromPeg, toPeg)] ++ hanoi (numberOfDisks - 1) tempPeg toPeg fromPeg
