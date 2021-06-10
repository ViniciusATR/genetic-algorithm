module Pso (
  Configuration (..),
  search
  ) where

import Data.List (minimumBy)
import System.Random
import RandomTools
import Control.Monad.State.Lazy
import Utils (rastriginCost)

--           CurrPosition   BestPosition CurrVelocity
type Solution = ([Double], [Double], [Double])


data Configuration = Configuration {
                                      randomSeed :: Int,
                                      solutionSize :: Int,
                                      maxIterations :: Int,
                                      solutionSetSize :: Int,
                                      globalBias :: Double,
                                      localBias :: Double,
                                      maxRateOfChange :: Double,
                                      infLimit :: Double,
                                      supLimit :: Double
                                   }


-- função de seno com amplitude reduzindo conforme X aumenta
-- muitos máximos locais
cost :: Double -> Double
cost x = (1/(1 + x)) * sin(2*pi*x)


generateSolution :: Configuration -> State StdGen Solution
generateSolution conf = do
  pos <- replicateM n $ randomDouble inf sup
  vel <- replicateM n $ randomDouble (-maxv) maxv
  return (pos, pos, vel)
    where
      n = solutionSize conf
      inf = infLimit conf
      sup = supLimit conf
      maxv = maxRateOfChange conf


validateRoC' :: Double -> Double -> Double
validateRoC' max roc
  | roc > max = max
  | roc < -max = -max
  | otherwise = roc


validateRoC :: Double -> [Double] -> [Double]
validateRoC max = map (validateRoC' max)

-- b  = melhor posição da particula
-- c = posição atual da particula
-- cv = velocidade atual da particula
-- gb = melhor posição global
modifySolution :: Configuration -> [Double] -> Solution -> State StdGen Solution
modifySolution conf gb (c, b, cv) = do
  r1 <- randomProbability
  r2 <- randomProbability
  let globalcoeff = zipWith (\x y -> gbias * r1 * (y - x)) c gb
  let localcoeff = zipWith (\x y -> lbias * r2 * (y - x)) c b
  let nv = validateRoC maxv $ zipWith3 (\a b c -> a + b + c) cv globalcoeff localcoeff
  let np = zipWith (+) c nv
  let nb = if rastriginCost np > rastriginCost b then np else b
  return (np, nb, nv)
    where
      inf = infLimit conf
      sup = supLimit conf
      gbias = globalBias conf
      lbias = localBias conf
      maxv = maxRateOfChange conf


getBestPosition :: [Solution] -> Solution
getBestPosition = minimumBy (\(a,_,_) (b,_,_) -> compare (rastriginCost a) (rastriginCost b))


search :: Configuration -> [Double]
search conf = evalState (search' conf initSet initBest 0) initGen
  where
    initGen = mkStdGen $ randomSeed conf
    initSet = evalState (replicateM setSize (generateSolution conf)) initGen
    setSize = solutionSetSize conf
    maxIter = maxIterations conf
    (initBest,_,_) = getBestPosition initSet
    search' :: Configuration -> [Solution] -> [Double] -> Int -> State StdGen [Double]
    search' conf currSet currBest currIter
      | maxIter <= currIter = return currBest
      | otherwise = do
        nextSet <- mapM (modifySolution conf currBest) currSet
        let (nextBest,_,_) = getBestPosition nextSet
        search' conf nextSet nextBest (currIter + 1)
