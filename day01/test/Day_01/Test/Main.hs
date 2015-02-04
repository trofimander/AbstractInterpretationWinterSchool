module Main where

import Day_01
import Test.Hspec


main :: IO ()
main = hspec $ do
  transitionSystemConvergesSpec
