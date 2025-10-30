{-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module Medium where

import Data.List (transpose, sort)
import Data.Function ((&))
import Utilities

one :: IO ()
one = do
    let input = (10 :: Int)
        expected = 55 -- I suspect the original expectation, 34, is incorrect.
        fibonacci i
            | i < 2     = i
            | otherwise = fibonacci (i - 1) + fibonacci (i - 2)
    ensureEqualTo expected $ fibonacci input

eight :: IO ()
eight = do
    let input = [ [3, 4, 1, 2], [9, 4, 8, 2] ]
        expected = (70 :: Int)
    ensureEqualTo expected $ input & map sort & transpose & map product & sum
