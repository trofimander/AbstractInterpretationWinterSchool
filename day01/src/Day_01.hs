module Day_01 where

import Test.Hspec

import Data.Set (Set, map, singleton, union)
import Data.Maybe (isNothing)


kleene_iterate:: Ord a => (a->a)->Int->a->Maybe a->Maybe a
kleene_iterate _ 0 _ _ = Nothing
kleene_iterate trans i state Nothing = kleene_iterate (trans) (i-1) (trans state) (Just state)
kleene_iterate trans i state (Just prev_s) = if prev_s == state then Just state else kleene_iterate (trans) (i-1) (trans state) (Just state)


lfp:: Ord a => (a->a)->a->Maybe a
lfp f s = kleene_iterate f 1000 s Nothing


state_converges:: Ord a => (a->a, a)->Bool
state_converges (trans, state) = not $ isNothing $ lfp trans state

next_state_set:: Ord a => (a->a)->Set a->Set a
next_state_set f s =  union s (Data.Set.map f s) 

set_converges:: Ord a =>(a->a, Set a)->Bool
set_converges (trans, set) = not $ isNothing $ lfp (next_state_set trans) set



-- (1) example of a transition system that converges

task1:: Int->Int
task1 x = div x 2



-- (2) an example of a transition system that doesn’t converge and whose reachable
--     states collecting semantics converges

task2:: Bool->Bool
task2 x = not x 


-- (3)  example of a transition system that doesn’t converge and whose reachable
--      states collecting semantics doesn’t converge

task3:: Int->Int
task3 x = x + 1



transitionSystemConvergesSpec :: Spec
transitionSystemConvergesSpec = do
  describe "transitionSystemConvergesSpec" $ do
    it "example of a transition system that converges" $ do
      state_converges (task1, 100) `shouldBe` True

    it "example of a transition system that doesn't converge" $ do
      state_converges (task2, False) `shouldBe` False

    it "reachable states collecting semantics converges" $ do
      set_converges (task2, singleton(False)) `shouldBe` True

    it "example of a transition system that doesn't converge" $ do
      state_converges (task3, 0) `shouldBe` False


    it "reachable states collecting semantics doesnt converge" $ do
      set_converges (task3, singleton(0)) `shouldBe` False





