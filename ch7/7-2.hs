myMap :: (a -> b) -> [a] -> [b]
myMap f xs = [f x | x <- xs]

-- myMap f [] = []
-- myMap f (x:xs) = f x : myMap f xs

myFilter :: (a -> Bool) -> [a] -> [a]
-- myFilter p [] = []
-- myFilter p (x : xs)
--     | p x = x : myFilter p xs
--     | otherwise = myFilter p xs
myFilter p xs = [x | x <- xs, p x]

sumsqreven :: [Int] -> Int
sumsqreven ns =
    sum (map (^ 2) (filter even ns))

