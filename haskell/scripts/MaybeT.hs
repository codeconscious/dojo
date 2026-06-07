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
import Data.Char (toUpper)
-- import Data.Function ((&))

main :: IO ()
main = do -- This is IO, not MaybeT IO.
    maybeStr <- runMaybeT v1 -- :: MaybeT IO String -> IO (Maybe String)
    putStrLn $ fromMaybe "何も入力されなかったため、文字数は0であるぞ。" maybeStr

v1 :: MaybeT IO String
v1 = do
    -- Note: We are inside the MaybeT IO monad, and the `do` block expects every line
    -- to be `MaybeT IO` (whether one of String or unit or other).
    lift $ putStrLn "何か文字を入力してくれ。" -- lift :: IO () -> MaybeT IO ()
    input <- lift getLine -- lift :: IO String -> MaybeT IO String
    guard (not $ null input) -- `guard` is already monad-aware and works in any MonadPlus (which MaybeT is).
    return $ "文字数は" ++ show (length input) ++ "である。" -- Applicativeのpureも可能で、StringをMaybeT IO Stringにliftする役割。

-- Other: Functions like guard, when, fail already "know" they're in a monad.
