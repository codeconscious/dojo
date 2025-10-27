{-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module Utilities (ensureEqualTo) where

import qualified Control.Monad

ensureEqualTo :: (Eq a, Show a) => a -> a -> IO ()
ensureEqualTo expected actual =
  Control.Monad.when
    (expected /= actual)
    $ putStrLn $ "NOT EQUAL! Expected: " ++ show expected ++ "\n           Actual:   " ++ show actual
