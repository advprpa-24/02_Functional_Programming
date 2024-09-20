-- {-# OPTIONS_GHC -Wall #-}
-- :set -Wall

-- Different ways to define data types in Haskell.

-- Enumeration types: 
data Color = R | O | G

walk :: Color -> Bool
walk R = False
walk O = True
walk G = True



-- Product types: 
data Person = MkP String Int deriving Show

p1 :: Person
p1 = MkP "Dani" 44

pname :: Person -> String
pname (MkP n _) = n

page :: Person -> Int
page (MkP _ a) = a

-- Record syntax: 
data PersonRec = MkPR {name :: String, age :: Int}

p2 = MkPR "Dani" 44
p3 = MkPR {name = "Dani", age = 44}


-- Sum types: 
data Lang 
    = Haskell String 
    | Java Int Bool

shortText :: Lang -> String
shortText (Haskell ver) = "H: " ++ ver
shortText (Java i _)    = "J: " ++ show i


-- Polymorphic types (generics)
data Result a = Error String | Success a

getOrDefault :: Result a -> a -> a
getOrDefault (Error _)   d = d
getOrDefault (Success a) _ = a

er = Error "Not found"
-- su = Success (MkP "" 12)


-- Recursive types
data Tree a 
    = Empty 
    | Branch (Tree a) a (Tree a)

t :: Tree Int
t = Branch Empty 1 (Branch Empty 2 Empty)

contains :: Int -> Tree Int -> Bool
contains _ Empty          = False
contains n (Branch l e r) = e == n || contains n l || contains n r


data Doc = Paragraph String | Chapter String [Doc]

d :: Doc
d = Chapter "02 FP" [Paragraph "FP <3", Chapter "Types" [Paragraph "We love types"]]