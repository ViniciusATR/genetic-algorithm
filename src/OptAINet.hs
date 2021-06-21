module OptAINet (
  Configuration (..),
  search
  ) where

import Data.List ( groupBy, sortBy, minimumBy, maximumBy,sort, tails )
import System.Random
import RandomTools
import Utils (rastriginCost)
import Control.Monad.State.Strict


data Configuration = Configuration {
                                     randomSeed :: Int,
                                     solutionSize :: Int,
                                     maxIterations :: Int,
                                     solutionSetSize :: Int,
                                     numClones :: Int,
                                     randomAdditions :: Int,
                                     fitnessThreshold :: Double,
                                     beta :: Double,
                                     fitness :: CostFunction
                                   }

type CostFunction = Solution -> Double
type Solution = [Double]


generateSolution :: Configuration -> State StdGen Solution
generateSolution c = replicateM n $ randomDouble (-5.12) 5.12
  where n = solutionSize c


distance :: Solution -> Solution -> Double
distance a b = sqrt $ sum sumOfsquares
  where
    sumOfsquares = zipWith (\x y -> (x - y)**2) a b


avgCost :: Configuration -> [Solution] -> Double
avgCost conf pop =  sum (map cost pop) / n
  where
    n = fromIntegral $ length pop
    cost = fitness conf

getBestSolution :: Configuration -> [Solution] -> Solution
getBestSolution conf = minimumBy (\a b -> compare (cost a) (cost b))
  where
    cost = fitness conf

getWorstSolution :: Configuration -> [Solution] -> Solution
getWorstSolution conf = maximumBy (\a b -> compare (cost a) (cost b))
  where
    cost = fitness conf

modifySolution :: Configuration -> Double -> Solution -> State StdGen Solution
modifySolution conf bestCost sol = do
  modVec <- replicateM size randomGaussian'
  let normalizedCost = 1.0 - cost sol/bestCost
  let alpha = (1.0/b) * exp (- normalizedCost)
  return $ zipWith (\s r -> s + r * alpha) sol modVec
    where
      b = beta conf
      cost = fitness conf
      size = solutionSize conf

cloneSelection :: Configuration -> Double -> Solution -> State StdGen Solution
cloneSelection conf bestCost s = do
  mutatedClones <- replicateM n (modifySolution conf bestCost s)
  return $ getBestSolution conf (mutatedClones ++ [s])
    where
      cost = fitness conf
      n = numClones conf

trimAndRefill :: Configuration -> [Solution] -> State StdGen [Solution]
trimAndRefill conf pop = do
  let neighbours = groupBy (\a b -> distance a b <= aff) pop
  trimmed <- mapM sample neighbours
  newCells <- replicateM ra (generateSolution conf)
  return $ trimmed ++ newCells
    where
      ra = randomAdditions conf
      aff = fitnessThreshold conf

searchStep :: Configuration -> [Solution] -> State StdGen [Solution]
searchStep conf pop = do
  let currAvg = avgCost conf pop
  let currBest = getBestSolution conf pop
  newGen <- mapM (cloneSelection conf (cost currBest)) pop
  let newAvg = avgCost conf newGen
  if newAvg < currAvg then do
    return newGen
  else do
    trimAndRefill conf newGen
    where
      cost = fitness conf

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
