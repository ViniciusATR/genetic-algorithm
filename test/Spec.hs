module Main where

import Onemax

import System.Random
import Control.Monad.State.Lazy
import Test.Tasty
import Test.Tasty.HUnit

gen = mkStdGen 120
conf = Configuration 120 4 20 10 2 0.99 0.99 0.99
initSet = evalState (replicateM 10 $ generateSolution conf) gen

main :: IO ()
main = do
  defaultMain onemaxTests


onemaxTests = testGroup "Testes do GA com Onemax" [ testCost , testGenerator, testSolutionModifier, testSolutionMixer, testSolutionSelector]

testCost = testGroup "Testes de função de custo"
           [
            testCase "Solução simples" (assertEqual "Solução simples" 5 (cost [True, True, True, False, True, False, True])),
            testCase "Tudo falso" (assertEqual "Tudo falso" 0 (cost [False, False, False]))
           ]


testGenerator = testGroup "Testes de função geradora"
                [
                 testCase "Solução de tamanho 4" (assertEqual "Tamanho 4" [True,True,False,False] (evalState (generateSolution conf) gen) )
                ]

testSolutionModifier = testGroup "Testes de modificador de soluções"
                       [
                        testCase "Modificar solução" (assertEqual "modificar Solução" [False, True, False, True] (evalState (modifySolution conf [True, False, True, False] ) gen))
                       ]

testSolutionMixer = testGroup "Teste de função de mistura de soluções"
                   [
                    testCase "Mistura ocorre" (assertEqual "mistura ocorre" [True, True, True, False] (evalState (mixSolutions conf ([True, True, True, True], [False, False, False, False])) gen))
                   ]

testSolutionSelector = testGroup "Teste de função de seletor de soluções"
                       [
                        testCase "Seleciona uma entre 10"(assertEqual "seleciona uma" [True, True, True, False]
                                                          (evalState (selectSolution conf initSet) gen))
                       ]
