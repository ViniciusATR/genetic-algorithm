import System.Environment
import OptAINet
import Utils (rastriginCost)


main :: IO ()
main = do
  args <- getArgs
  let solutionSize = read $ head args :: Int
  let maxIterations = read $ args!!1 :: Int
  let conf = Configuration 42 solutionSize maxIterations 20 5 10 0.1 1.0 rastriginCost
  let result = search conf
  print result
  let cost = rastriginCost result
  print cost
