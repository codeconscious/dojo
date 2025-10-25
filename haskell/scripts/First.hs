-- My first-ever Haskell script, written on the morning of 2025-10-25!
-- I wrote it as a conversion of a small F# module I wrote.
import Data.List (transpose, sort)
import Data.Function ((&))

main :: IO ()
main = do
  let input = [ [3, 4, 1, 2], [9, 4, 8, 2] ]

  -- Application operator: right-associative function application
  print $ sum $ map product $ transpose $ map sort input

  -- Reverse application operator (from Data.Function): identical to the pipeline operator!
  input & map sort & transpose & map product & sum & print

  -- Function composition operator: right-to-left function composition
  let f = sum . map product . transpose . map sort
  print $ f input
