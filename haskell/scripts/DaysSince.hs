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
import Data.Function ((&))
import System.FilePath
import Data.Char (toLower)

-- main :: IO ()
-- main = do
--     safeArg <- checkArgs
--     either putStrLn process safeArg
--   where
--     process fileName = do
--         maybeContent <- readSmallFile fileName
--         either putStrLn handleContent maybeContent

--     handleContent content = do
--         let lineCount = length $ T.lines content
--         putStrLn $ "This file has " ++ show lineCount ++ " line(s)."

main :: IO ()
main =
    runExceptT computation >>= either putStrLn return
    where
        computation = do
            fileName <- ExceptT checkArgs
            content <- ExceptT $ readSmallFile fileName
            liftIO $ do
                let lineCount = show $ length $ T.lines content
                    charCount = show $ T.length content
                putStrLn $ "This file has " ++ lineCount ++ " line(s) and " ++ charCount ++ " character(s)."

checkArgs :: IO (Either String FilePath)
checkArgs = do
    args <- getArgs
    return $
        case args of
        []    -> Left "You must provide the name of a plain-text file as an argument."
        [arg] -> Right arg
        _     -> Left "Too many arguments! Provide only the name of a plain-text file containing dates."

checkExtension :: FilePath -> Either [Char] FilePath
checkExtension p
    | extension == ".txt" = Right p
    | otherwise = Left $ "Invalid file extension: " ++ extension
    where
        extension = p & takeExtension & map toLower

readSmallFile :: FilePath -> IO (Either [Char] T.Text)
readSmallFile filePath = do
    result <- try (T.readFile filePath) :: IO (Either IOException T.Text)
    return $
        case result of
        Left exn -> Left ("Error reading file: " ++ show exn)
        Right content -> Right content
