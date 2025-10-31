{-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module Easy where

import Data.Function ((&))
import Data.Maybe (mapMaybe)
import Data.List
import Data.Char (toLower)
import Data.Ord
import Utilities

one :: IO ()
one = do
    let input = ["this", "is", "an", "array"]
        expected = [4, 2, 2, 5]
    map length input & ensureEqualTo expected

two :: IO ()
two = do
    let input = [1, 2, 3, 4, 5]
        expected = (15 :: Int)
    sum input & ensureEqualTo expected

three :: IO ()
three = do
    let expected = "Hello, Candidate!"
        greeter greeting = \candidate -> greeting ++ ", " ++ candidate ++ "!" -- "Redundant lambda"
    greeter "Hello" "Candidate" & ensureEqualTo expected

five :: IO ()
five = do
    let input = "abccba"
        expected = True
    input == reverse input & ensureEqualTo expected

eight :: IO ()
eight = do
    let input = (15 :: Int)
        expected = "fizz buzz fizz fizz buzz fizz fizzbuzz" -- Fixed incorrect output from the source.
        fizzbuzz i | mod i 15 == 0 = Just "fizzbuzz"
                   | mod i 5  == 0 = Just "buzz"
                   | mod i 3  == 0 = Just "fizz"
                   | otherwise = Nothing
    [1..input] & mapMaybe fizzbuzz & unwords & ensureEqualTo expected

nine :: IO ()
nine = do
    let input = ["fynn", "nyfn"]
        expected = True
    input & map sort & nub & length & (==) 1 & ensureEqualTo expected

ten :: IO ()
ten = do
    let input = "Aloha! My name is Fynn."
        expected = 6
        isVowel ch = ch `elem` ['a', 'e', 'i', 'o', 'u']
    input & map toLower & filter isVowel & length & ensureEqualTo expected

eleven :: IO ()
eleven =
    target `elem` input & ensureEqualTo expected
    where
        input = ["my", "hello", "fynn", "name", "is"]
        target = "fynn"
        expected = True

twelve :: IO ()
twelve =
    input & map toLower & words & mostFrequent & ensureEqualTo expected
    where
        input = "hello my name is fynn and this is kind of funny. Is this real?" -- 句読点は無視してもOK
        expected = "is"
        mostFrequent x = x & sort & group & maximumBy (comparing length) & head

fourteen :: IO ()
fourteen =
    input & nub & ensureEqualTo expected
    where
        input = [1 :: Int,2,2,3,4,3,2]
        expected = [1,2,3,4]

fifteen :: IO ()
fifteen = do
    let input = [2, 3, 4]
        expected = 24 :: Int
    input & foldl' (*) 1 & ensureEqualTo expected

sixteen :: IO ()
sixteen = do
    let input = "Hello my name is Fynn!!"
        expected = "Hello"
        isAllowedChar ch = ch `elem` concat (['a'..'z'] : ['A'..'Z'] : [[' ']])
    input & filter isAllowedChar & words & maximumBy (comparing length) & ensureEqualTo expected
    -- ensureEqualTo expected $ maximumBy (comparing length) $ words $ filter isAllowedChar input

seventeen :: IO ()
seventeen = do
    let input = (4 :: Int)
        expected = (10 :: Int)
    [input, input-1 .. 1] & foldl' (+) 0 & ensureEqualTo expected

eighteen :: IO ()
eighteen = do
    let input = "Hello my name is Fynn"
        expected = "helo mynaisf"
    input & map toLower & nub & ensureEqualTo expected

twentyOne :: IO ()
twentyOne =
    reverse input & ensureEqualTo expected
    where input = "Fynn"; expected = "nnyF"

twentyTwo :: IO ()
twentyTwo =
    [input,input-1..1] & foldl' (*) 1 & ensureEqualTo expected
    where
        input = 4 :: Int
        expected = 24
