-- Problems from https://github.com/0x66796e6e/interview-preparation/blob/master/markdown/

module Main where

import qualified Control.Monad
import qualified Easy
import qualified Medium

ensureEqualTo :: (Eq a, Show a) => a -> a -> IO ()
ensureEqualTo actual expected =
  Control.Monad.when
    (actual /= expected)
    $ putStrLn $ "NOT EQUAL! Actual: " ++ show actual ++ " / Expected: " ++ show expected

main :: IO ()
main = sequence_ [
    Easy.one,
    Easy.two,
    Easy.three,
    Easy.five,
    Medium.eight
  ]
