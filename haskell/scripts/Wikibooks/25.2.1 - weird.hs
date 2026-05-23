-- https://en.wikibooks.org/wiki/Haskell/Other_data_structures
module Weird where

data Weird a b = First a
               | Second b
               | Third [(a, b)]
               | Fourth (Weird a b)

weirdMap :: (a -> c) -> (b -> d) -> Weird a b -> Weird c d
weirdMap fa fb = go
    where
        go (First x) = First (fa x)
        go (Second y) = Second (fb y)
        -- go (Third z) = Third (map (\(j, k) -> (fa j, fb k)) z)
        go (Third z) = Third [ (fa j, fb k) | (j, k) <- z ]
        go (Fourth w) = Fourth (go w)

{- NOTES:
   `g` is a locally defined helper function in the where clause.
   Only the first two variables (fa and fb) are passed, and this makes `g` a
   partally applied function (Weird a b -> Weird c d) using the still-unused
   third parameter. It also captures fa and fb, bring them into scope.
   The fourth case's recusion is also much cleaner.
 -}

weirdFold :: (a -> c) -> (b -> c) -> ([(a,b)] -> c) -> (c -> c) -> Weird a b -> c
weirdFold f1 f2 f3 f4 = g
  where
    g (First x)  = f1 x
    g (Second y) = f2 y
    g (Third z)  = f3 z
    g (Fourth w) = f4 (g w)
