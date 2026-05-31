
-- SOURCE: https://nauths.fr/en/2026/05/28/practical-use-of-monads.html

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
import Text.Printf (printf)
import Data.Function ((&))

shout :: String -> String
shout = map toUpper

main :: IO ()
main = exercise1v1

-- Exercise 1: Write using `>>=`.
exercise1v0 :: IO ()
exercise1v0 = do
  putStrLn "Please input your name:"
  userName <- getLine
  putStrLn $ "Hello " ++ userName ++ "!"

exercise1v1 :: IO ()
exercise1v1 = putStrLn "Please input your name:" >> getLine >>= (\userName -> putStrLn $ "Hello " ++ userName ++ "!")

exercise1v2 :: IO ()
exercise1v2 = putStrLn "Please input your name:" >> getLine >>= printf "Hello %s!\n"

-- Exercise 2: Rewrite it with >>=, and then with do.
foo0 :: [Int] -> [Int]
foo0 l = [x + 5 | x <- l, even x]

foo1 :: [Int] -> [Int]
foo1 l = map (+5) $ filter even l

foo2 :: [Int] -> [Int]
foo2 l = do
    x <- l
    if even x then return (x + 5) else []
