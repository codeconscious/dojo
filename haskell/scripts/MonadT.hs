-- {-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- Suppress such warnings.
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module MonadTransformers where

-- import qualified Data.Text.IO as T
-- import qualified Data.Text as T
-- import System.FilePath
import Control.Exception (IOException, try)
import Control.Monad.Trans.Maybe
import Control.Monad.Trans (lift, liftIO)
import Control.Monad (guard)
import Data.Maybe (fromMaybe)
-- import Data.Function ((&))

main :: IO ()
main = do
    maybeStr <- runMaybeT v1
    putStrLn $ fromMaybe "Nothing was entered!" maybeStr

v1 :: MaybeT IO String
v1 = do
    lift $ putStrLn "Enter any text:"
    input <- lift getLine
    guard (not $ null input)
    pure $ "Your text: " ++ input

-- v1' :: IO (Maybe String)
-- v1' = do
--     putStrLn "Enter any text:"
--     input <- getLine
--     guard (not $ null input)
--     Just $ "Your text: " ++ input -- Fails.
