{-# OPTIONS_GHC -Wall -Werror #-} -- Treat warnings as errors.
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-} -- Suppress such warnings.
{-# HLINT ignore "Redundant lambda" #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}

-- import Data.List (transpose, sort)
-- import Data.Function ((&))

class Squareable a where
    square :: a -> a

instance Squareable Int where
    square a = a * a

instance Squareable (Int, Int) where
    square (a, b) = (square a, square b)

instance Squareable String where
    square a = a ++ a

class Modifiable a where
    prefix :: a -> a -> a
    suffix :: a -> a -> a

instance Modifiable String where
    prefix p a = p ++ ": " ++ a
    suffix s a = a ++ " (" ++ s ++ ")"

newtype JNum = JNum Int

instance Show JNum where
    show (JNum a) = show a ++ " is a great number." -- "はすばらしい数字です。"

main :: IO ()
main = do
    print $ square (6 :: Int)
    print $ square (3:: Int, 4:: Int)
    print $ square "hello"
    print $ prefix "BEHOLD" "hello"
    print $ suffix "BEHOLD" "hello"
    print $ show $ JNum 10
    -- getLine >>= \line -> putStrLn ("You entered \"" ++ line ++"\"")
