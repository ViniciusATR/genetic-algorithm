import System.Environment
import OptAINet
import Utils (rastriginCost)


main :: IO ()
main = do
  args <- getArgs
  let solutionSize = read $ head args :: Int
  let seed = read $  args!!1 :: Int
  let maxIterations = read $ args!!2 :: Int
  let randomAddPercent = read $ args!!3 :: Double
  let beta = read $ args!!4 :: Double
  let fitThresh = read $ args!!5 :: Double
  let popSize = div 10000 maxIterations
  let randomAdd = round $ fromIntegral popSize * randomAddPercent
  let conf = Configuration seed solutionSize maxIterations popSize 10 randomAdd fitThresh beta rastriginCost
  let result = search conf
  print result
  let cost = rastriginCost result
  print cost
