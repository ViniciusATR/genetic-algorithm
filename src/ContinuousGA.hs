module ContinuousGA (
  Configuration (..),
  rouletteSelection,
  tournamentSelection,
  elitistSelection,
  search
  ) where

import Data.List ( sortBy, minimumBy, sort, tails )
import System.Random
import RandomTools
import Utils (rastriginCost)
import Control.Monad.State.Strict


data Configuration = Configuration {
                                     randomSeed :: Int,
                                     solutionSize :: Int,
                                     maxIterations :: Int,
                                     solutionSetSize :: Int,
                                     cutoff :: Int, -- number of solutions to take from population in selection
                                     solutionModProbability :: Double,
                                     solutionMixProbability :: Double,
                                     selector :: Selector,
                                     fitness :: CostFunction
                                   }

type CostFunction = Solution -> Double
type Solution = [Double]
type Selector = Configuration -> [Solution] -> State StdGen Solution

generateSolution :: Configuration -> State StdGen Solution
generateSolution c = replicateM n $ randomDouble (-5.12) 5.12
  where n = solutionSize c

modifySolution :: Configuration -> Solution -> State StdGen Solution
modifySolution c s = do
  rand <- randomProbability
  if rand < probM then do
    modVec <- replicateM size randomGaussian'
    alpha  <- randomProbability
    return $ zipWith (\s r -> s + r * alpha) s modVec
  else
    return s
  where
      probM = solutionModProbability c
      cost = fitness c
      size = solutionSize c


aritMix :: Int -> (Solution, Solution) -> State StdGen Solution
aritMix idx (s1, s2) = do
  alpha <- randomProbability
  let firstSection = take ( idx + 1 ) s1
  let s1' = drop (idx + 1) s1
  let s2' = drop (idx + 1) s2
  let secondSection = zipWith (\x y -> alpha * x + (1 - alpha) * y) s1' s2'
  return $ firstSection ++ secondSection


mixSolutions :: Configuration -> (Solution, Solution) -> State StdGen Solution
mixSolutions c (s1, s2) = do
    rand <- randomProbability
    if rand < probMix then do
        position <- randomInt 0 (size - 1)
        aritMix position (s1, s2)
    else do
      chooseRand <- randomBool
      return $ if chooseRand then s1 else s2
    where
      probMix = solutionMixProbability c
      size = solutionSize c


getBestSolution :: Configuration -> [Solution] -> Solution
getBestSolution conf = minimumBy (\a b -> compare (cost a) (cost b))
  where
    cost = fitness conf


elitistSelection :: Selector
elitistSelection c set = do
    let bestCandidates = take cut $ sortBy (\a b -> compare (cost a) (cost b)) set
    randIndx <- randomInt 0 (cut - 1)
    return $ bestCandidates!!randIndx
      where
        cut = cutoff c
        cost = fitness c


rouletteSelection :: Selector
rouletteSelection c set = do
  let fitness = map rastriginCost set
  let cumsum  = sum fitness
  let roulette =  map (\x -> 1.0 - x/cumsum) fitness
  randomWeightedChoice $ zip set roulette


tournamentSelection :: Selector
tournamentSelection c set = do
  participants <- replicateM size $ sample set
  return $ last $ scanl1 (\x y -> if cost x < cost y then x else y) participants
    where
      size = cutoff c
      cost = fitness c


searchStep :: Configuration -> [Solution] -> State StdGen [Solution]
searchStep conf set = do
  bestSolutions <- replicateM setSize $ selectSolution conf set
  let parentPairs = take setSize $ [(a, b) | (a: bs) <- tails bestSolutions, b <- bs]
  newSolutions <- mapM (mixSolutions conf) parentPairs
  mapM (modifySolution conf) newSolutions
    where
      setSize = solutionSetSize conf
      selectSolution = selector conf


search :: Configuration -> Solution
search conf = getBestSolution conf $ evalState (search' conf initSet 0) initGen
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
