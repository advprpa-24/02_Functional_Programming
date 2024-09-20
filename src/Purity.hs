{- Purity -}
import Prelude hiding (sum)

sum :: Int -> Int
sum 0 = 0
sum n = n + sum (n-1)

p :: Int
p = sum 3 + sum 3

p' = 6 + sum 3

p'' = sum 3 + 6

p''' = 6 + 6


p'''' = 2 * sum 3


f :: Int -> Int
f n = sum n + sum n