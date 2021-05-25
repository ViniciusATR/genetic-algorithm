module RandomTools(
    randomState,
    randomProbability,
    randomDouble,
    randomBool,
    randomInt,
    randomGaussian,
    randomGaussian',
    shuffle,
    sample,
    randomWeightedChoice
  ) where


import System.Random
import Control.Monad.State.Lazy
import qualified Data.Map as M

randomState :: (StdGen -> (a, StdGen)) -> State StdGen a
randomState randomFunction = do
    s <- get
    let (a, s') = randomFunction s
    put s'
    return a


randomProbability :: State StdGen Double
randomProbability = randomState $ randomR (0.0::Double, 1.0::Double)


randomDouble :: Double -> Double -> State StdGen Double
randomDouble min max = randomState $ randomR (min::Double, max::Double)


randomBool :: State StdGen Bool
randomBool = randomState $ randomR (True, False)


randomInt :: Int -> Int -> State StdGen Int
randomInt min max = randomState $ randomR (min::Int, max::Int)


-- Implementação de um shuffle de listas de acordo com
-- http://okmij.org/ftp/Haskell/perfect-shuffle.txt
-- Obtido também em https://wiki.haskell.org/Random_shuffle

fisherYatesStep :: RandomGen g => (M.Map Int a, g) -> (Int, a) -> (M.Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((M.insert j x . M.insert i (m M.! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen

fisherYates :: RandomGen g => [a] -> g -> ([a], g)
fisherYates [] gen = ([], gen)
fisherYates l gen =
  toElems $ foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (M.elems x, y)
    numerate = zip [1..]
    initial x gen = (M.singleton 0 x, gen)

shuffle :: [a] -> State StdGen [a]
shuffle xs = randomState $ fisherYates xs


randomWeightedChoice :: [(a, Double)] -> State StdGen a
randomWeightedChoice sample = do
  shuffled <- shuffle sample
  let weights = map snd sample
  let minWeight = minimum weights
  let maxWeight = maximum weights
  cutoff <- randomDouble minWeight maxWeight
  let chosen = head $ dropWhile ((<cutoff) . snd) shuffled
  return $ fst chosen


sample :: [a] -> State StdGen a
sample xs = do
  idx <- randomInt 0 (n - 1)
  return $ xs !! idx
    where n = length xs

-- Implementação de números aleatórios de uma dist normal
-- a partir de: http://hackage.haskell.org/package/normaldistribution-1.1.0.3
-- utilizando o método de boxMuller
boxMuller :: Floating a => a -> a -> (a, a)
boxMuller u1 u2 = (r * cos t, r * sin t)
  where
    r = sqrt (-2 * log u1)
    t = 2 * pi * u2


gaussian :: (RandomGen g, Random a, Floating a) => a -> a -> g -> (a, g)
gaussian sd m g = (n1 * sd + m, g2)
  where
    (u1, g1) = randomR (0, 1) g
    (u2, g2) = randomR (0, 1) g1
    (n1, n2) = boxMuller u1 u2


randomGaussian :: (Floating a, Random a) => a -> a -> State StdGen a
randomGaussian sd m = randomState $ gaussian sd m


randomGaussian' :: (Floating a, Random a) => State StdGen a
randomGaussian' = randomState $ gaussian 1.0 0.0
