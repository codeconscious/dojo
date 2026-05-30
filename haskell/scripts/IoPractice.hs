-- {-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- Suppress such warnings.
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module DaysSince where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Exception (IOException, try)
import System.Environment (getArgs)
import Control.Monad.Except
import Control.Monad.IO.Class
import System.FilePath
import Data.Char (toLower, toUpper)
import Data.Bifunctor (first)
-- import Data.Function ((&))

shout :: String -> String
shout = map toUpper

main :: IO ()
main = v2

v1 :: IO ()
v1 = do
    putStrLn "Enter a string:"
    input <- getLine
    let asUpper = shout input
    putStrLn asUpper

v2 :: IO ()
v2 = putStrLn "Enter a string:" >> getLine >>= (putStrLn . shout)

v3 :: IO ()
v3 = putStrLn "Enter a string:" >> fmap shout getLine >>= putStrLn
