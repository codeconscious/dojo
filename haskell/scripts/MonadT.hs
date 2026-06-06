
-- SOURCE: https://nauths.fr/en/2026/05/28/practical-use-of-monads.html

-- {-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- Suppress such warnings.
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module MonadTransformers where

-- import qualified Data.Text.IO as T
-- import qualified Data.Text as T
import Control.Exception (IOException, try)
import Control.Monad.Trans.Maybe
import System.FilePath
import Control.Monad.Trans (lift, liftIO)
import Control.Monad (guard)
import Data.Maybe
-- import Data.Function ((&))

main :: IO ()
main = do
    result <- runMaybeT v1
    putStrLn $ fromMaybe "Nothing was entered!" result

v1 :: MaybeT IO String
v1 = do
    lift $ putStrLn "Enter any text:"
    input <- lift getLine
    guard (not (null input))
    return $ "Your text: " ++ input
