module Onemax
    ( Solution,
      Configuration (..),
      cost,
      generateSolution,
      modifySolution,
      mixSolutions,
      selectSolution
    ) where

import Data.List (sortBy, tails)
import System.Random
import RandomTools
import Control.Monad.State.Strict

data Configuration = Configuration {
                                     randomSeed :: Int,
                                     solutionSize :: Int,
                                     maxIterations :: Int,
                                     solutionSetSize :: Int,
                                     cutoff :: Int, -- number of solutions to take from population in selection
                                     mutationProbability :: Double,
                                     solutionModProbability :: Double,
                                     solutionMixProbability :: Double
                                   }
type Solution = [Bool]

cost :: Solution -> Int
cost s = length (filter id s)


generateSolution :: Configuration -> State StdGen Solution
generateSolution c = replicateM n randomBool
  where n = solutionSize c


flipBoolean :: Configuration -> Bool -> State StdGen Bool
flipBoolean c b = do
    rand <- randomProbability
    return $ if rand < prob then not b else b
      where prob = mutationProbability c


modifySolution :: Configuration -> Solution -> State StdGen Solution
modifySolution c s = do
    rand <- randomProbability
    if rand < probM then
      mapM (flipBoolean c) s
    else
      return s
    where probM = solutionModProbability c


mixSolutions :: Configuration -> (Solution, Solution) -> State StdGen Solution
mixSolutions c (s1, s2) = do
    rand <- randomProbability
    if rand < probMix then do
        position <- randomInt 0 (size - 1)
        let firstSection = take (position + 1) s1
        let secondSection = drop (position + 1) s2
        return $ firstSection ++ secondSection
    else do
      chooseRand <- randomBool
      return $ if chooseRand then s1 else s2
    where
      probMix = solutionMixProbability c
      size = solutionSize c


selectSolution :: Configuration -> [Solution] -> State StdGen Solution
selectSolution c set = do
    let bestCandidates = take cut $ reverse $sortBy (\a b -> compare (cost a) (cost b)) set
    randIndx <- randomInt 0 (cut - 1)
    return $ bestCandidates!!randIndx
      where cut = cutoff c


searchStep :: Configuration -> [Solution] -> State StdGen [Solution]
searchStep conf set = do
  bestSolutions <- replicateM setSize $ selectSolution conf set
  let parentPairs = take setSize $ [(a, b) | (a: bs) <- tails bestSolutions, b <- bs]
  newSolutions <- mapM (mixSolutions conf) parentPairs
  mapM (modifySolution conf) newSolutions
    where
      setSize = solutionSetSize conf

search :: Configuration -> Solution
search conf = head $ evalState (search' conf initSet 0) initGen
  where
    initGen = mkStdGen $ randomSeed conf
    initSet = evalState (replicateM setSize (generateSolution conf)) initGen
    setSize = solutionSetSize conf
    maxIter = maxIterations conf
    search' conf currSet currIter
      | maxIter <= currIter = return currSet
      | otherwise = do
          nextSet <- searchStep conf currSet
          search' conf nextSet (currIter + 1)
