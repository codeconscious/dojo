{-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- Suppress such warnings.
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module DaysSince where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Exception (IOException, try)
import GHC.Internal.System.Environment (getArgs)

main :: IO ()
main = do
    safeArg <- checkArgs
    case safeArg of
        Left err -> putStrLn err
        Right arg -> do
            maybeContent <- readSmallFile arg
            case maybeContent of
                Left err -> putStrLn err
                Right content -> do
                    let lineCount = length (T.lines content)
                    putStrLn $ "File has " ++ show lineCount ++ " lines!"

checkArgs :: IO (Either String String)
checkArgs = do
    args <- getArgs
    return $
        case args of
        [] -> Left "You must enter the filename!"
        [arg] -> Right arg
        _ -> Left "Too many args! Enter only the filename."

readSmallFile :: FilePath -> IO (Either [Char] T.Text)
readSmallFile filepath = do
    result <- try (T.readFile filepath) :: IO (Either IOException T.Text)
    return $
        case result of
        Left ex -> Left ("Error reading file: " ++ show ex)
        Right content -> Right content
