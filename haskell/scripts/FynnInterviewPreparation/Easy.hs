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
import Control.Arrow ((>>>))
-- import Utilities
import qualified Control.Monad

ensureEqualTo :: (Eq a, Show a) => a -> a -> IO ()
ensureEqualTo expected actual =
    Control.Monad.when
        (expected /= actual)
        $ putStrLn $ "NOT EQUAL! Expected: " ++ show expected ++ "\n           Actual:   " ++ show actual

safeIndex :: Int -> [a] -> Maybe a
safeIndex _ [] = Nothing
safeIndex n (x:xs)
    | n < 0 = Nothing
    | n == 0 = Just x
    | otherwise = safeIndex (n - 1) xs

one :: IO ()
one = do
    input & map length & ensureEqualTo expected
    where
        input = ["this", "is", "an", "array"]
        expected = [4, 2, 2, 5]

two :: IO ()
two = do
    let input = [1, 2, 3, 4, 5]
        expected :: Int = 15
    sum input & ensureEqualTo expected

three :: IO ()
three = do
    greet "Hello" "Candidate" & ensureEqualTo expected
    where
        greet greeting name = greeting ++ ", " ++ name ++ "!"
        expected = "Hello, Candidate!"

five :: IO ()
five = do
    let input = "abccba"
        expected = True
    input == reverse input & ensureEqualTo expected

eight :: IO ()
eight = do
    [1..input] & mapMaybe fizzbuzz & unwords & ensureEqualTo expected
    where
        input :: Int = 15
        expected = "fizz buzz fizz fizz buzz fizz fizzbuzz" -- Fixed incorrect output from the source.
        fizzbuzz i | mod i 15 == 0 = Just "fizzbuzz"
                   | mod i 5  == 0 = Just "buzz"
                   | mod i 3  == 0 = Just "fizz"
                   | otherwise = Nothing

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
    input & normalizeWords & mostFrequent & ensureEqualTo expected
    where
        input = "hello my name is fynn and this is kind of funny. Is this real?" -- 句読点は無視してもOK
        expected = "is"
        normalizeWords = map toLower >>> words
        mostFrequent = head . maximumBy (comparing length) . group . sort
        -- mostFrequent = sort >>> group >>> maximumBy (comparing length) >>> head

fourteen :: IO ()
fourteen =
    nub input & ensureEqualTo expected
    where
        input :: [Int] = [1,2,2,3,4,3,2]
        expected = [1,2,3,4]

fifteen :: IO ()
fifteen = do
    foldl' (*) 1 input & ensureEqualTo expected
    where input = [2, 3, 4]
          expected :: Int = 24

sixteen :: IO ()
sixteen = do
    input & filter isAllowedChar & words & maximumBy (comparing length) & ensureEqualTo expected
    -- ensureEqualTo expected $ maximumBy (comparing length) $ words $ filter isAllowedChar input
    where input = "Hello my name is Fynn!!"
          expected = "Hello"
          allowedChars = concat (['a'..'z'] : ['A'..'Z'] : [[' ']])
          isAllowedChar ch = ch `elem` allowedChars

seventeen :: IO ()
seventeen = do
    sum [input, input-1 .. 1] & ensureEqualTo expected
    where
        input :: Int = 4
        expected :: Int = 10

eighteen :: IO ()
eighteen = do
    let input = "Hello my name is Fynn"
        expected = "helo mynaisf"
    input & map toLower & nub & ensureEqualTo expected

twenty :: IO ()
twenty = do
    let sampleInput :: [Int] = [1, 2, 3]
        expected    :: [Int] = [1, 2, 3, 1, 2, 3]
    (sampleInput ++ sampleInput) & ensureEqualTo expected

twentyOne :: IO ()
twentyOne =
    reverse input & ensureEqualTo expected
    where input = "Fynn"; expected = "nnyF"

twentyTwo :: IO ()
twentyTwo =
    product [input, input-1..1] & ensureEqualTo expected
    where
        input :: Int = 4
        expected = 24

twentyThree :: IO ()
twentyThree = do
    let sampleInput :: [Int] = [40, 3, 100, -5]
        sortedInput = sort sampleInput
        indexes = [0, 3, 70, -10]
        expected = [Just (-5), Just 100, Nothing, Nothing]
    -- indexes & map (\i -> safeIndex i sortedInput) & ensureEqualTo expected
    indexes & map (`safeIndex` sortedInput) & ensureEqualTo expected
