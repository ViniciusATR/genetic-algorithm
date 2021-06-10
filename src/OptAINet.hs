module OptAINet (
  Configuration (..),
  search
  ) where

import Data.List ( groupBy, sortBy, minimumBy, sort, tails )
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
type Selector = Configuration -> [Solution] -> State StdGen Solution



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

mutate :: Double -> Solution -> State StdGen Solution
mutate alpha = mapM (mutate' alpha)
    where
      mutate' alpha d = do
        rand <- randomGaussian'
        return $ d + rand * alpha

cloneSelection :: Configuration -> Solution -> State StdGen Solution
cloneSelection conf s = do
  let clones = replicate n s
  let alpha = (1.0/b) * exp (- cost s)
  mutatedClones <- mapM (mutate alpha) clones
  return $ getBestSolution conf mutatedClones ++ s
    where
      cost = fitness conf
      n = numClones conf
      b = beta conf


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
  newGen <- mapM (cloneSelection conf) pop
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
