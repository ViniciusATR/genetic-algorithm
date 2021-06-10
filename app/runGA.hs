import System.Environment
import ContinuousGA
import Utils (rastriginCost)


main :: IO ()
main = do
  args <- getArgs
  let selection = head args
  let solutionSize = read $ args!!1 :: Int
  let maxIterations = read $ args!!2 :: Int
  let conf | selection == "elitist" = Configuration 42 solutionSize maxIterations 20 5 0.05 1.0 0.95 0.90 elitistSelection rastriginCost
           | selection == "roulette" = Configuration 42 solutionSize maxIterations 20 5 0.05 1.0 0.95 0.90 rouletteSelection rastriginCost
           | selection == "tournament" = Configuration 42 solutionSize maxIterations 20 5 0.05 1.0 0.95 0.90 tournamentSelection rastriginCost
  let result = search conf
  print result
  let cost = rastriginCost result
  print cost
