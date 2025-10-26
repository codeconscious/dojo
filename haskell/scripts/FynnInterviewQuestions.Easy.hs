-- Problems from https://github.com/0x66796e6e/interview-preparation/blob/master/markdown/easy/easy-js-questions.md!

{-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}

-- Haskell doesn't support multiple or nested modules!
module FynnInterviewQuestions.Easy where

-- import Data.List (transpose, sort)
import Data.Function ((&))
import qualified Control.Monad

ensureEqualTo :: (Eq a, Show a) => a -> a -> IO ()
ensureEqualTo actual expected =
  Control.Monad.when (actual /= expected) $ putStrLn $ "NOT EQUAL! Actual: " ++ show actual ++ " / Expected: " ++ show expected

one :: IO ()
one = do
  let input = ["this", "is", "an", "array"]
  let expected = [4, 2, 2, 5]
  map length input & ensureEqualTo expected

two :: IO ()
two = do
    let input = [1, 2, 3, 4, 5]
    let expected = (15 :: Integer)
    sum input & ensureEqualTo expected

three :: IO ()
three = do
    let expected = "Hello, Candidate!"
    let greeter greeting = \candidate -> greeting ++ ", " ++ candidate ++ "!" -- "Redundant lambda"
    greeter "Hello" "Candidate" & ensureEqualTo expected

five :: IO ()
five = do
    let input = "abccba"
    let expected = True
    input == reverse input & ensureEqualTo expected

main :: IO ()
main = sequence_ [one, two, three, five]
