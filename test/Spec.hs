module Main where

import Lib
import Onemax

import Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main = do
  defaultMain onemaxTests


onemaxTests = (testGroup "Testes do Onemax" [ testCost testGenerator testSolutionModifier testSolutionMixer testSolutionSelector ])

testCost = testGroup "Testes de função de custo"
           [
            testCase "Simple Solution" (assertEqual "Simple Solution" 5 (cost [True, True, True, False, True, False, True])),
            testCase "All False" (assertEqual "All False" 0 (cost [False, False, False]))
           ]
