{-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- Suppress such warnings.
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

-- import Data.List (transpose, sort)
-- import Data.Function ((&))

class Squareable a where
    square :: a -> a

instance Squareable Int where
    square a = a * a

instance Squareable (Int, Int) where
    square (a, b) = (square a, square b)

instance Squareable String where
    square a = a ++ a

class Prefixable a where
    prefix :: a -> a -> a
    suffix :: a -> a -> a

instance Prefixable String where
    prefix a p = p ++ ": " ++ a
    suffix a s = a ++ " (" ++ s ++ ")"

main :: IO ()
main = do
    print $ square (6 :: Int)
    print $ square (3:: Int, 4:: Int)
    print $ square "hello"
    print $ prefix "hello" "BEHOLD"
    print $ suffix "hello" "BEHOLD"
