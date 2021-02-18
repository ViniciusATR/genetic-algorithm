module Onemax
    ( Solution,
      Configuration (..),
      cost,
      generateSolution,
      modifySolution,
      mixSolutions,
      -- selectSolution
    ) where

import Data.List (sortBy)
import System.Random
import Control.Monad.State.Lazy

data Configuration = Configuration { solutionSize :: Int,
                                     mutationProbability :: Double,
                                     solutionModProbability :: Double,
                                     solutionSetSize :: Int,
                                     solutionMixProbability :: Double,
                                     cutoff :: Int -- number of solutions to take from population in selection
                                   }
type Solution = [Bool]

cost :: Solution -> Int
cost s = length (filter id s)


randomState :: (StdGen -> (a, StdGen)) -> State StdGen a
randomState randonFunction = do
    s <- get
    let (a, s') = randonFunction s
    put s'
    return a


randomBool :: State StdGen Bool
randomBool = randomState $ randomR (True, False)


randomProbability :: State StdGen Double
randomProbability = randomState $ randomR (0.0::Double, 1.0::Double)


randomInt :: Int -> Int -> State StdGen Int
randomInt min max = randomState $ randomR (min::Int, max::Int)


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


mixSolutions :: Configuration -> Solution -> Solution -> State StdGen Solution
mixSolutions c s1 s2 = do
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
    let bestCandidates = take cut $ sortBy (\a b -> compare (cost a) (cost b)) set
    randIndx <- randomInt 0 (cut - 1)
    return $ bestCandidates!!randIndx
      where cut = cutoff c
