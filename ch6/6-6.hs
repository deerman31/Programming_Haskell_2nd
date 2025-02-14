myProduct :: (Num a) => [a] -> a
myProduct [] = 1
myProduct (n : ns) = n * myProduct ns

myProduct2 :: (Num a) => [a] -> a
myProduct2 xs = foldr (*) 1 xs

myDrop1 :: (Eq t, Num t) => t -> [t] -> [t]
myDrop1 0 xs = xs
myDrop1 _ [] = []
myDrop1 n (_ : xs) = myDrop1 (n - 1) xs

myInit :: [a] -> [a]
myInit [_] = []
myInit (x : xs) = x : myInit xs

-- myInit (x : xs)
--     | null xs = []
--     | otherwise = x : myInit xs