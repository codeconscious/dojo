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
import Data.Char (toLower)
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Time
import Data.Maybe
import Data.Either (rights)
-- import Data.Function ((&))

data RowSummary = RowSummary {
      category  :: Text
    , summary   :: Text
    , date      :: Day
    , daysSince :: Integer
} deriving (Show)

main :: IO ()
main =
    runExceptT computation >>= either putStrLn return
    where
        computation :: ExceptT String IO () = do
            fileName <- checkArgs
            checkExtension fileName
            content <- readSmallFile' fileName
            liftIO $ do
                let lines_    = T.lines content
                    lineCount = show $ length lines_
                    charCount = show $ T.length content
                    output    = fmap rights . mapM runExceptT . fmap parseLine
                putStrLn $ "This file has " ++ lineCount ++ " line(s) and " ++ charCount ++ " character(s)."
                results <- output lines_
                mapM_ print results

checkArgs :: ExceptT String IO FilePath
checkArgs = do
    args <- liftIO getArgs
    case args of
        []    -> throwError "You must provide the name of a CSV as an argument."
        [arg] -> return arg
        _     -> throwError "Too many arguments! Provide only the name of a CSV containing dates."

checkExtension :: FilePath -> ExceptT String IO ()
checkExtension path
    | isSupportedExt = return ()
    | otherwise      = throwError $ "Invalid file extension: " ++ ext
    where
        ext = map toLower $ takeExtension path
        isSupportedExt = ext == ".csv"

readSmallFile :: FilePath -> ExceptT String IO T.Text
readSmallFile filePath = do
    result <- liftIO (try (T.readFile filePath) :: IO (Either IOException T.Text))
    case result of
        Left exn   -> throwError $ "Error reading file: " ++ show exn
        Right text -> return text

readSmallFile' :: FilePath -> ExceptT String IO T.Text
readSmallFile' filePath = do
    result <- liftIO $ try @IOException (T.readFile filePath)
    liftEither $ first (("Error reading file: " ++) . show) result

parseLine :: Text -> ExceptT String IO RowSummary
parseLine text = do
    now <- liftIO $ utctDay <$> getCurrentTime
    case T.splitOn separator text of
        [c, s, d] -> return $ RowSummary c (T.strip s) (toDay d) (diffDays now (toDay d))
        _         -> throwError "Failed to parse line"
    where
        separator = T.pack "," :: Text
        toDay d = read $ T.unpack d :: Day
        -- now = utctDay <$> getCurrentTime
        -- daysSince x = diffDays x now
