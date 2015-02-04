module CounterMatch.Eval where

import Data.Maybe
import CounterMatch.Model

type Result = [State]

data State = State {x::Int, y::Int, z::Int, pc:: Int} deriving (Eq, Show)


step:: Program->State->State
step p s@(State x y z pc) =
            (case p !! (pc-1) of
            INC X -> State (x+1) y z (pc+1)
            INC Y -> State x (y+1) z (pc+1)
            INC Z -> State x y (z+1) (pc+1)
            DEC X -> if x>0 then State (x-1) y z (pc+1) else State x y z pc
            DEC Y -> if y>0 then State x (y-1) z (pc+1) else State x y z pc
            DEC Z -> if z>0 then State x y (z-1) (pc+1) else State x y z pc
            ZERO X pc' pc'' -> State x y z (if x == 0 then pc' else pc'')
            ZERO Y pc' pc'' -> State x y z (if y == 0 then pc' else pc'')
            ZERO Z pc' pc'' -> State x y z (if z == 0 then pc' else pc'')
            STOP -> s
            )

kleene_iterate::  (State->State)->Int->State->Maybe State->Result
kleene_iterate _ 0 _ _ = []
kleene_iterate trans i state Nothing = kleene_iterate trans (i-1) (trans state) (Just state)
kleene_iterate trans i state (Just prev_s) = prev_s: (if prev_s == state then [] else (kleene_iterate (trans) (i-1) (trans state) (Just state)))


lfp:: (State->State)->State->Result
lfp f s = kleene_iterate f 20 s Nothing


eval:: Program->State->Result
eval p s = lfp (step p) s

