-- {-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- Suppress such warnings.
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
{- HLINT ignore "Collapse lambdas" -}

module Medium where

import Data.List (transpose, sort)
import Data.Function ((&))
-- import Utilities
import qualified Control.Monad

ensureEqualTo :: (Eq a, Show a) => a -> a -> IO ()
ensureEqualTo expected actual =
    Control.Monad.when
        (expected /= actual)
        $ putStrLn $ "NOT EQUAL! Expected: " ++ show expected ++ "\n           Actual:   " ++ show actual

one :: IO ()
one = do
    fibonacci input & ensureEqualTo expected
    where
        input :: Int = 10
        expected = 55 -- I suspect the original listed expectation, 34, is incorrect.
        fibonacci i
            | i < 2     = i
            | otherwise = fibonacci (i - 1) + fibonacci (i - 2)

three :: IO ()
three = do
        input & words & map reverse & unwords & ensureEqualTo expected
    where
        input = "Hello my name is Fynn"
        expected = "olleH ym eman si nnyF"

four :: IO ()
four = do
    f 2 3 4 & ensureEqualTo expected
    where
        f = \a -> \b -> \c -> (a :: Int) * b * c
        expected = 2 * 3 * 4

eight :: IO ()
eight = do
    let input = [ [3, 4, 1, 2], [9, 4, 8, 2] ]
        expected :: Int = 70
    input & map sort & transpose & map product & sum & ensureEqualTo expected
