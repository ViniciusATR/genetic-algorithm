module Onemax
    ( Solution,
      cost,
      generateSolution,
      modifySolution,
      mixSolutions,
      selectSolution
    ) where

type Solution = [Bool]

cost :: Solution -> Integer
cost s = length (filter (\x -> x) a)
