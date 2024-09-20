isort :: [Int] -> [Int]
isort []     = []
isort (i:is) = insert i (isort is)

insert :: Int -> [Int] -> [Int]
insert e [] = [e]
insert e (i:is) 
  | e <= i    = e : i : is
  | otherwise = i : insert e is

