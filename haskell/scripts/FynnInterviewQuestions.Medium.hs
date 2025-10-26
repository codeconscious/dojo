-- Haskell doesn't support multiple or nested modules!
module FynnInterviewQuestions.Medium where

import Data.List (transpose, sort)
import Data.Function ((&))

ensureEqual :: (Eq a, Show a) => a -> a -> IO ()
ensureEqual actual expected =
  if actual /= expected
    then print $ "NOT EQUAL! Actual: " ++ show actual ++ " / Expected: " ++ show expected
    else return ()

eight :: IO ()
eight = do
  let input = [ [3, 4, 1, 2], [9, 4, 8, 2] ]
  let expected = 70
  ensureEqual expected $ input & map sort & transpose & map product & sum

main :: IO ()
main = eight
