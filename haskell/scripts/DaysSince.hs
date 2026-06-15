{-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- Suppress such warnings.
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module DaysSince where

import qualified Data.Text as T
import qualified Data.Text.IO as T
-- import qualified Data.Map.Strict as Map
import Control.Exception (IOException, try)
import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.IO.Class
import Data.Bifunctor (first)
import Data.Char (toLower)
import Data.Either (partitionEithers)
-- import Data.Function ((&))
import Data.List (sortBy)
import Data.Text (Text)
import Data.Time
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.FilePath
import Text.Read (readEither)

data RowSummary = RowSummary {
      category  :: String
    , summary   :: String
    , date      :: Day
    , daysSince :: Integer
}

instance Show RowSummary where
    show (RowSummary c s d ds) = c ++ " | " ++ s ++ " | " ++ show d ++ " | " ++ show ds

main :: IO ()
main =
    runExceptT computation >>= either putStrLn return
    where
        computation :: ExceptT String IO () = do
            fileName <- checkArgs
            checkExtension fileName
            content <- readSmallFile' fileName
            let lines_    = T.lines content
                lineCount = show $ length lines_
                charCount = show $ T.length content
                results   = traverse (runExceptT . parseLine) lines_ -- 同じ: mapM runExceptT . fmap parseLine
            liftIO $ do
                putStrLn $ "This file has " ++ lineCount ++ " line(s) and " ++ charCount ++ " character(s)."
                (errors, summaries) <- partitionEithers <$> results
                printSummaries summaries
                printErrors errors

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

-- readSmallFile :: FilePath -> ExceptT String IO T.Text
-- readSmallFile filePath = do
--     result <- liftIO (try (T.readFile filePath) :: IO (Either IOException T.Text))
--     case result of
--         Left exn   -> throwError $ "Error reading file: " ++ show exn
--         Right text -> return text

readSmallFile' :: FilePath -> ExceptT String IO T.Text
readSmallFile' filePath = do
    result <- liftIO $ try @IOException (T.readFile filePath)
    liftEither $ first (("Error reading file: " ++) . show) result

parseLine :: Text -> ExceptT String IO RowSummary
parseLine text = do
    now <- liftIO $ utctDay <$> getCurrentTime
    case T.splitOn separator text of
        [c, s, d] ->
            let parsedDay = readEither $ T.unpack d :: Either String Day in
            case parsedDay of
                Left err  -> throwError $ "* Error parsing date \"" ++ T.unpack d ++ "\" in line with category " ++ show (T.unpack c) ++ " and summary " ++ show (T.unpack s) ++ ": `" ++ err ++ "`"
                Right day -> return $ RowSummary (T.unpack c) (T.unpack $ T.strip s) day (diffDays now day)
        _ -> throwError $ "* Error parsing malformed line: " ++ T.unpack text
    where
        separator = T.pack "," :: Text

printSummaries :: [RowSummary] -> IO ()
printSummaries summaries = do
    mapM_ print $ sortBy (comparing category) summaries

printErrors :: [String] -> IO ()
printErrors errs = do
    unless (null errs) $ do
        putStrLn $ "There were " ++ show (length errs) ++ " parse error(s)."
        mapM_ putStrLn errs

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex f xs = [f i x | (i, x) <- zip [0..] xs]
