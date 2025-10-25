-- My first-ever Haskell, written on the morning of 2025-10-25!
import Data.List (transpose, sort)
import Data.Function ((&))

main :: IO ()
main = do
  let input = [ [3, 4, 1, 2], [9, 4, 8, 2] ]

  let output = sum $ map product $ transpose $ map sort $ input
  print output

  let output' = input & map sort & transpose & map product & sum
  print output'

  let calc = sum . map product . transpose . map sort
  print (calc input)
