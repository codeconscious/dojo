{-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module Medium where

import Data.List (transpose, sort)
import Data.Function ((&))
import Utilities

eight :: IO ()
eight = do
  let input = [ [3, 4, 1, 2], [9, 4, 8, 2] ]
      expected = (70 :: Integer)
  ensureEqualTo expected $ input & map sort & transpose & map product & sum
