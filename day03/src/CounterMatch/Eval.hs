module CounterMatch.Eval where

import Data.Maybe
import CounterMatch.Model
import CounterMatch.Parse
import CounterMatch.Parity

import Data.Functor
import Data.Map as Map (Map, fromList, singleton, foldl, findWithDefault, union, insert, foldWithKey, unionWith, toAscList)
import Data.List as List (foldl)


data ParityState = ParityState {x:: Parity, y:: Parity, z:: Parity}
        deriving (Eq)

instance Show ParityState where
    show (ParityState x y z) = "x:" ++ show x ++ ", y:" ++ show y ++ ", z:" ++ show z


type State = Map Int ParityState

showMap:: Map Int ParityState -> String
showMap a = List.foldl (\r (k, v) -> r ++ "\n" ++ show k ++ "-> " ++ show v) [] (Map.toAscList a)

bottom_par = ParityState BOTTOM BOTTOM BOTTOM

bottom_hat:: Int->ParityState
bottom_hat _ = bottom_par


unionParity:: ParityState->ParityState->ParityState
unionParity (ParityState x y z) (ParityState x' y' z')= ParityState (join x x') (join y y')  (join z z')

unionState:: State->State->State
unionState s1 s2 = unionWith unionParity s1 s2


apply:: VAR->(Parity->Parity)->ParityState->ParityState
apply X f (ParityState x y z) = ParityState (f x) y z
apply Y f (ParityState x y z) = ParityState x (f y) z
apply Z f (ParityState x y z) = ParityState x y (f z)

transform:: Program->Int->Map Int ParityState->Map Int ParityState
transform p i m = case (p !! (i-1)) of
                     INC v -> Map.singleton (i+1) (apply v incr ps)
                     DEC v -> Map.singleton (i+1) (apply v decr ps)
                     ZERO v pc pc' -> unionState
                             (Map.singleton pc (apply v is_zero ps))
                             (Map.singleton pc'(apply v not_zero ps))
                     _ -> Map.fromList []

                  where ps = findWithDefault bottom_par i m
                        ParityState x y z = ps





bottom_map:: Program->Map Int ParityState
bottom_map p = List.foldl (\r e -> unionState r (Map.singleton e (bottom_hat e))) (Map.fromList []) [1..(length p)]

init_map:: Program->Map Int ParityState->Map Int ParityState
init_map p m = Map.insert 1 (ParityState TOP EVEN EVEN) m

update_map:: Program->Map Int ParityState->Map Int ParityState
update_map p m = List.foldl (\r e -> unionState r (transform p e m)) (init_map p (bottom_map p)) [1..length p]

step:: Program->Map Int ParityState->Map Int ParityState
step p m = update_map p m

kleene_iterate::  (State->State)->Int->State->Maybe State->State
kleene_iterate _ 0 state _ = state
kleene_iterate trans i state Nothing = kleene_iterate trans (i-1) (trans state) (Just state)
kleene_iterate trans i state (Just prev_s) = if prev_s == state then state else (kleene_iterate (trans) (i-1) (trans state) (Just state))


lfp:: (State->State)->State->State
lfp f s = kleene_iterate f 1000 s Nothing


eval:: Program->State->State
eval p s = lfp (step p) s

