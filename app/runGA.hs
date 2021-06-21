import System.Environment
import ContinuousGA
import Utils (rastriginCost)


main :: IO ()
main = do
  args <- getArgs
  let selection = head args
  let seed= read $ args!!1 :: Int
  let solutionSize = read $ args!!2 :: Int
  let maxIterations = read $ args!!3 :: Int
  let cutoff = read $ args!!4 :: Int
  let mutationProb = read $ args!!5 :: Double
  let crossoverProb = 1.0 - mutationProb
  let popSize = div 10000 maxIterations
  let conf | selection == "elitist" = Configuration seed solutionSize maxIterations popSize cutoff mutationProb crossoverProb elitistSelection rastriginCost
           | selection == "roulette" = Configuration seed solutionSize maxIterations popSize cutoff mutationProb crossoverProb rouletteSelection rastriginCost
           | selection == "tournament" = Configuration seed solutionSize maxIterations popSize cutoff mutationProb crossoverProb tournamentSelection rastriginCost
           | otherwise = Configuration seed solutionSize maxIterations popSize cutoff mutationProb crossoverProb tournamentSelection rastriginCost
  let result = search conf
  let cost = rastriginCost result
  print cost
