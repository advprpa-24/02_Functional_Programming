{- Lazy Evaluation -}

-- Termination
infinite :: Integer
infinite = 1 + infinite

a :: Integer
a = fst (0, infinite)


-- Infinite lists
ones :: [Integer]
ones = 1 : ones

b :: Integer
b = head ones

c :: [Integer]
c = take 5 ones

-- iterate :: (a -> a) -> a -> [a]
d :: [Integer]
d = iterate (+1) 0

iterate' :: (a -> a) -> a -> [a]
iterate' f z = a : iterate f a
    where a = f z

e :: [(Integer, String)]
e = zip d ["Apples", "Carrots", "Salad"]


-- Control structures
ifThenElse :: Bool -> a -> a -> a
ifThenElse True  t _ = t
ifThenElse False _ e = e

f :: Double -> Double
f x = ifThenElse (x /= 0) (100/x) 0

g :: Double
g = f 12

h :: Double
h = f 0