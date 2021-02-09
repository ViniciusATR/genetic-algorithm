module Main where

import Lib
import Onemax

import System.Random
import Control.Monad.State.Lazy
import Test.Tasty
import Test.Tasty.HUnit

gen = mkStdGen 120

main :: IO ()
main = do
  defaultMain onemaxTests


onemaxTests = (testGroup "Testes do Onemax" [ testCost , testGenerator, testSolutionModifier])
-- testSolutionMixer testSolutionSelector ])

testCost = testGroup "Testes de função de custo"
           [
            testCase "Solução simples" (assertEqual "Solução simples" 5 (cost [True, True, True, False, True, False, True])),
            testCase "Tudo falso" (assertEqual "Tudo falso" 0 (cost [False, False, False]))
           ]


testGenerator = testGroup "Testes de função geradora"
                [
                 testCase "Solução de tamanho 5" (assertEqual "Tamanho 5" [True,True,False,False,True] (evalState (generateSolution 5) gen) )
                ]

testSolutionModifier = testGroup "Testes de modificador de soluções"
                       [
                        testCase "Modificar solução" (assertEqual "modificar Solução" [False, True, False, True] (evalState (modifySolution 0.9 0.2 [True, False, True, False] ) gen))
                       ]
