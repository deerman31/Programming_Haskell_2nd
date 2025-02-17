-- foldr (+) 0 [1, 2, 3]
-- 1 + (2 + (3 + 0))

-- foldl (+) 0 [1, 2, 3]
-- ((0 + 1) + 2) + 3

mySum :: (Num a) => [a] -> a
mySum = sum' 0
  where
    sum' v [] = v
    sum' v (x : xs) = sum' (v + x) xs

mySum2 :: (Num a) => [a] -> a
mySum2 = foldl (\acc x -> x + acc) 0

myLength1 :: [a] -> Int
myLength1 = aux 0
  where
    aux v [] = v
    aux v (_ : xs) = aux (1 + v) xs

myLength2 :: [a] -> Int
myLength2 = foldl (\n _ -> n + 1) 0

myReverse :: [a] -> [a]
myReverse = foldl (\acc x -> x : acc) []

-- myReverse xs = aux [] xs
--   where
--     aux v [] = v
--     aux v (x : xs) = aux (x : v) xs

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f v [] = v
myFoldl f v (x : xs) = myFoldl f (f v x) xs