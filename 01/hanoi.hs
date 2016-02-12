{-# OPTIONS_GHC -Wall #-}

module Hanoi where

type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi numberOfDisks pegA pegB pegC
  | numberOfDisks == 1 = [(pegA, pegB)]
  | numberOfDisks == 2 = [(pegA, pegC), (pegA,pegB), (pegC,pegB)]
