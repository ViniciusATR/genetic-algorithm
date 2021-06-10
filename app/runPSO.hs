import System.Environment
import Pso
import Utils (rastriginCost)


main :: IO ()
main = do
  args <- getArgs
  let solutionSize = read $ head args :: Int
  let maxIterations = read $ args!!1 :: Int
  let conf = Configuration 42 solutionSize maxIterations 20 1.0 1.0 1.0 (-5.12) 5.12
  let result = search conf
  print result
  let cost = rastriginCost result
  print cost
