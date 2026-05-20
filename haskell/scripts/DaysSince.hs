{-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
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

main :: IO ()
main = do
    safeArg <- checkArgs
    either putStrLn process safeArg
  where
    process fileName = do
        maybeContent <- readSmallFile fileName
        either putStrLn handleContent maybeContent

    handleContent content = do
        let lineCount = length $ T.lines content
        putStrLn $ "This file has " ++ show lineCount ++ " line(s)."

main' :: IO ()
main' =
    runExceptT computation >>= either putStrLn return
    where
        computation = do
            fileName <- ExceptT checkArgs
            content <- ExceptT $ readSmallFile fileName
            liftIO $ do
                let lineCount = length $ T.lines content
                putStrLn $ "This file has " ++ show lineCount ++ " line(s)."

checkArgs :: IO (Either String String)
checkArgs = do
    args <- getArgs
    return $
        case args of
        [] -> Left "You must provide the filename as an argument."
        [arg] -> Right arg
        _ -> Left "Too many arguments! Provide only the filename containing dates."

readSmallFile :: FilePath -> IO (Either [Char] T.Text)
readSmallFile filepath = do
    result <- try (T.readFile filepath) :: IO (Either IOException T.Text)
    return $
        case result of
        Left exn -> Left ("Error reading file: " ++ show exn)
        Right content -> Right content
