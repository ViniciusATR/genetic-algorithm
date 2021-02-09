module Onemax
    ( Solution,
      cost,
      generateSolution,
      modifySolution,
      -- mixSolutions,
      -- selectSolution
    ) where

import System.Random
import Control.Monad.State.Lazy


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


generateSolution :: Int -> State StdGen Solution
generateSolution n = replicateM n randomBool


flipBoolean :: Double -> Bool -> State StdGen Bool
flipBoolean prob b = do
    rand <- randomProbability
    return $ if rand < prob then b else not b


modifySolution :: Double -> Double -> Solution -> State StdGen Solution
modifySolution probM indProb s = do
    rand <- randomProbability
    if rand < probM then
      mapM (flipBoolean indProb) s
    else
      return s

