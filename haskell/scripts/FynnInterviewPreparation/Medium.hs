{-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module Medium where

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
