module Utils (
  rastriginCost
  )where

type CostFunction = Solution -> Double
type Solution = [Double]

-- Função de rastrigin para vetores n dimensionais
-- Com o número 10 e x entre [-5.12, 5.12] tem mínimo em
-- x = 0 com f(x) = 0
-- O sinal é alterado para continuar um problema de maximização
rastriginCost :: CostFunction
rastriginCost xs = 10 * n + cumsum
  where
    n = fromIntegral $ length xs
    inner = map (\x -> x^2 - 10 * cos (2 * pi * x)) xs
    cumsum = sum inner
