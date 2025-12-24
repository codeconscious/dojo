{-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- Suppress such warnings.
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

module Functors where
import Data.Bitraversable (bimapM)


maybe :: Maybe Int
maybe =
    let maybe1 = Just "Hello" :: Maybe String
        -- maybe2 = Just 6 :: Maybe Int
        -- maybe3 = Nothing :: Maybe Bool
        f1 = length :: String -> Int
        -- f2 = (* 2) :: Int -> Int
        -- f3 = show :: Bool -> String
    in fmap f1 maybe1

example3 :: (String, Int) -> Either String (String, String)
example3 = bimapM
    (\str -> if null str
             then Left "Empty string error"
             else Right (str ++ " validated"))
    (\num -> if num < 0
             then Left "Negative number error"
             else Right (show num ++ " validated"))
