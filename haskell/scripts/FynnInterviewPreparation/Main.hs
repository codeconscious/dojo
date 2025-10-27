-- Problems from https://github.com/0x66796e6e/interview-preparation/blob/master/markdown/

module Main where

import qualified Control.Monad
import qualified Easy
import qualified Medium

ensureEqualTo :: (Eq a, Show a) => a -> a -> IO ()
ensureEqualTo expected actual =
  Control.Monad.when
    (expected /= actual)
    $ putStrLn $ "NOT EQUAL! Expected: " ++ show expected ++ "\n           Actual:   " ++ show actual

main :: IO ()
main = sequence_ [
    Easy.one,
    Easy.two,
    Easy.three,
    Easy.five,
    Easy.eight,
    Easy.nine,
    Easy.ten,
    Easy.eleven,
    Medium.eight
  ]
