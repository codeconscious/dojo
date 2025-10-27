{-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module Easy where

import Data.Function ((&))
import qualified Control.Monad
import Data.Maybe (mapMaybe)
import Data.List

-- TODO: Export to a new utility module.
ensureEqualTo :: (Eq a, Show a) => a -> a -> IO ()
ensureEqualTo expected actual =
  Control.Monad.when
    (expected /= actual)
    $ putStrLn $ "NOT EQUAL! Expected: " ++ show expected ++ "\n           Actual:   " ++ show actual

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

eight :: IO ()
eight = do
    let input = (15 :: Int)
        expected = "fizz buzz fizz fizz buzz fizz fizzbuzz" -- Fixed incorrect output from the source.
        fizzbuzz i | mod i 15 == 0 = Just "fizzbuzz"
                   | mod i 5  == 0 = Just "buzz"
                   | mod i 3  == 0 = Just "fizz"
                   | otherwise = Nothing
    [1..input] & mapMaybe fizzbuzz & unwords & ensureEqualTo expected

nine :: IO ()
nine = do
    let input = ["fynn", "nyfn"]
    let expected = True
    input & map sort & nub & length & (==) 1 & ensureEqualTo expected
