module Main where

-- import qualified MyLib (someFunc)

main :: IO ()
main = do
  print "Who are you?"
  name <- getLine
  let greeting = "Hi there, " ++ name ++ "!"
  print greeting

  -- MyLib.someFunc
