module CounterMatch.Model where

import Data.List

data VAR = X | Y | Z deriving (Show)
data Instruction = INC VAR | DEC VAR | ZERO VAR Int Int | STOP deriving (Show)

type Program = [Instruction]


