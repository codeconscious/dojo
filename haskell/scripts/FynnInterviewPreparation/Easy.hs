{-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module Easy where

import Data.Function ((&))
import qualified Control.Monad

ensureEqualTo :: (Eq a, Show a) => a -> a -> IO ()
ensureEqualTo actual expected =
  Control.Monad.when (actual /= expected) $ putStrLn $ "NOT EQUAL! Actual: " ++ show actual ++ " / Expected: " ++ show expected

one :: IO ()
one = do
    let input = ["this", "is", "an", "array"]
        expected = [4, 2, 2, 5]
    map length input & ensureEqualTo expected

two :: IO ()
two = do
    let input = [1, 2, 3, 4, 5]
        expected = (15 :: Int)
    sum input & ensureEqualTo expected

three :: IO ()
three = do
    let expected = "Hello, Candidate!"
    let greeter greeting = \candidate -> greeting ++ ", " ++ candidate ++ "!" -- "Redundant lambda"
    greeter "Hello" "Candidate" & ensureEqualTo expected

five :: IO ()
five = do
    let input = "abccba"
        expected = True
    input == reverse input & ensureEqualTo expected
