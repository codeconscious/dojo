-- For use in GHCI.
import Data.List
import Data.Traversable

-- Reverse a list using a fold.
-- https://youtu.be/46dksIrx6jQ?list=PLe7Ei6viL6jGp1Rfu0dil1JH1SHk9bgDV
rev' :: [a] -> [a]
rev' xs = foldl' (\acc x -> x:acc) [] xs
-- rev' = foldl' (\acc x -> x:acc) []
