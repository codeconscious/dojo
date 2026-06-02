isAsc' :: [Int] -> Bool
isAsc' [] = True
isAsc' [_] = True
isAsc' [x,y] = x <= y
-- isAsc' (x:y:xs) = if x <= y then isAsc' (y : xs) else False
isAsc' (x:y:xs) = (x <= y) && isAsc' (y : xs)

isAsc'' :: [Int] -> Bool
isAsc'' [] = True
isAsc'' [_] = True
isAsc'' [x,y] = x <= y
isAsc'' (x:y:xs) = (x <= y) && isAsc' (y : xs)
