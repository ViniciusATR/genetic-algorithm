module Pso (

  ) where

import System.Random
import Control.Monad.State.Lazy

--           CurrPosition   BestPosition CurrVelocity
type Solution = (Double, Double, Double)


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
cost x = (1/(1 + x)) * sin((2*pi*x))


randomState :: (StdGen -> (a, StdGen)) -> State StdGen a
randomState randonFunction = do
    s <- get
    let (a, s') = randonFunction s
    put s'
    return a


randomProbability :: State StdGen Double
randomProbability = randomState $ randomR (0.0::Double, 1.0::Double)


randomDouble :: Double -> Double -> State StdGen Double
randomDouble min max = randomState $ randomR (min::Double, max::Double)


generateSolution :: Configuration -> State StdGen Solution
generateSolution conf = do
  x <- randomDouble inf sup
  vx <- randomDouble (-maxv) maxv
  return (x, x, vx)
    where
      inf = infLimit conf
      sup = supLimit conf
      maxv = maxRateOfChange conf


validateRoC :: Double -> Double -> Double
validateRoC max roc
  | roc > max = max
  | roc < -max = -max
  | otherwise = roc


-- bx  = melhor posição da particula
-- cx = posição atual da particula
-- cvx = velocidade atual da particula
-- gbx = melhor posição global
modifySolution :: Configuration -> Double -> Solution -> State StdGen Solution
modifySolution conf gbx (cx, bx, cv) = do
  r1 <- randomProbability
  r2 <- randomProbability
  let nv = validateRoC maxv $ cv + gb * r1 * (gbx - cx) + lb * r2 * (bx - cx)
  let nx = cx + nv
  let nbx = if cost nx > cost bx then nx else bx
  return (nx, nbx, nv)
    where
      inf = infLimit conf
      sup = supLimit conf
      gb = globalBias conf
      lb = localBias conf
      maxv = maxRateOfChange conf


getBestPosition :: [Solution] -> Double
getBestPosition set = foldr (\(_,x,_) y -> if cost x > cost y then x else y) 0 set


search :: Configuration -> Double
search conf = evalState (search' conf initSet initBest 0) initGen
  where
    initGen = mkStdGen $ randomSeed conf
    initSet = evalState (replicateM setSize (generateSolution conf)) initGen
    setSize = solutionSetSize conf
    maxIter = maxIterations conf
    initBest = getBestPosition initSet
    search' :: Configuration -> [Solution] -> Double -> Int -> State StdGen Double
    search' conf currSet currBest currIter
      | maxIter <= currIter = return currBest
      | otherwise = do
        nextSet <- mapM (modifySolution conf currBest) currSet
        let nextBest = getBestPosition nextSet
        search' conf nextSet nextBest (currIter + 1)
