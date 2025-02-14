import Data.Time (iso8601DateFormat)
insert x [] = [x]
insert x (y : ys) =
    if x <= y
        then x : y : ys
        else y : insert x ys

-- insert2 x [] = [x]
-- insert2 x (y : ys)
--     | x <= y = x : y : ys
--     | otherwise = y : insert x ys

isort [] = []
isort (x:xs) = insert x (isort xs)