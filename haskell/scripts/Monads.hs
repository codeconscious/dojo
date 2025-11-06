{-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- Suppress such warnings.
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

-- import Data.List (transpose, sort)
-- import Data.Function ((&))

guarded :: Bool -> [a] -> [a]
guarded True  xs = xs
guarded False _  = [] -- Empty lists are automatically discarded by the list monad's behavior
                      -- and contribute nothing to the final result!

multiplyTo :: Int -> [(Int, Int)]
multiplyTo n = do
  x <- [1..n]
  y <- [x..n]
  guarded (x * y == n) $
    return (x, y)

letters :: [String]
letters = do
    x <- ['a'..'c']
    y <- ['X'..'Z']
    -- return [x: [y]] -- Results in [[[Char]]]
    return [x, y] -- Identical to (x : [y])!

main :: IO ()
main = do
    print $ multiplyTo 100
    print $ [1 :: Int, 2, 3] >>= \x -> [4, 5, 6] >>= \y -> [x*y]
    -- print $ ['a'..'z'] >>= \x -> ['A'..'Z'] >>= \y -> [[x] ++ [y]]
    print $ ['a'..'c'] >>= \x -> ['X'..'Z'] >>= \y -> [x : [y]]
    print letters
    print $ [[x,y] | x <- ['a'..'c'], y <- ['X'..'Z']] -- Comprehension
