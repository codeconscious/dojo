{-# OPTIONS_GHC -Wall -Werror #-}

-- Haskell doesn't support multiple or nested modules!
module FynnInterviewQuestions.Medium where

import Data.List (transpose, sort)
import Data.Function ((&))
import qualified Control.Monad

ensureEqual :: (Eq a, Show a) => a -> a -> IO ()
ensureEqual actual expected =
  Control.Monad.when (actual /= expected) $ putStrLn $ "NOT EQUAL! Actual: " ++ show actual ++ " / Expected: " ++ show expected

eight :: IO ()
eight = do
  let input = [ [3, 4, 1, 2], [9, 4, 8, 2] ]
      expected = (70 :: Integer)
  ensureEqual expected $ input & map sort & transpose & map product & sum

main :: IO ()
main = eight
