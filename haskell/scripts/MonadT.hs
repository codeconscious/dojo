
-- SOURCE: https://nauths.fr/en/2026/05/28/practical-use-of-monads.html

-- {-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- Suppress such warnings.
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module MonadTransformers where

import qualified Data.Text.IO as T
import qualified Data.Text as T
import Control.Exception (IOException, try)
import Control.Monad.Trans.Maybe
import System.Environment (getArgs)
import Control.Monad.Except
import Control.Monad.IO.Class
import System.FilePath
import Text.Printf (printf)
import Control.Monad.Trans (lift, liftIO)
import Data.Function ((&))
import Control.Monad (msum)

main :: IO ()
main = do
    result <- runMaybeT v1
    case result of
       Just str -> putStrLn str
       Nothing -> putStrLn "Nothing!" -- Never hit.

v1 :: MaybeT IO String
v1 = do
    lift $ putStrLn "Enter any text:"
    input <- lift getLine
    return $ "Your text: " ++ input
