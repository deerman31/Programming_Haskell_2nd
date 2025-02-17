mySum :: (Num a) => [a] -> a
mySum = foldr (+) 0

-- vは基底値
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f v [] = v
myFoldr f v (x : xs) = f x (myFoldr f v xs)

myLength :: [a] -> Int
-- myLength [] = 0
-- myLength (_ : xs) = 1 + myLength xs
myLength = foldr (\_ n -> 1 + n) 0

myReverse :: [a] -> [a]
snoc x xs = xs ++ [x]
myReverse = foldr snoc []


-- foldr (+) 0 [1, 2, 3]
-- 1 + (2 + (3 + 0))