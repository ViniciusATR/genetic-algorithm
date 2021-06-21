import System.Environment
import Pso
import Utils (rastriginCost)


main :: IO ()
main = do
  args <- getArgs
  let solutionSize = read $ head args :: Int
  let seed = read $ args!!1
  let maxIterations = read $ args!!2 :: Int
  let gbias = read $ args!!3 :: Double
  let lbias = read $ args!!4 ::Double
  let popSize = div 10000 maxIterations
  let conf = Configuration seed solutionSize maxIterations popSize 1.0 gbias lbias (-5.12) 5.12
  let result = search conf
  let cost = rastriginCost result
  print cost
