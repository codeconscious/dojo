{-# OPTIONS_GHC -Wall -Werror #-}

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

main :: IO ()
main = do
    one
    two
