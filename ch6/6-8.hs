-- 1
fac :: Int -> Int
fac n
    | n < 0 = 0
    | n == 0 = 1
    | otherwise = n * fac (n - 1)

-- 2
sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n - 1)

-- 3
(^^^) :: Int -> Int -> Int
m ^^^ 0 = 1
m ^^^ n = m * (m ^^^ (n - 1))

-- 4
euclid :: Int -> Int -> Int
euclid n1 n2
    | n1 == n2 = n1
    | n1 < n2 = euclid (n2 - n1) n1
    | otherwise = euclid (n1 - n2) n2

-- 6
myAnd :: [Bool] -> Bool
myAnd [] = True
myAnd (b : bs)
    | not b = False
    | otherwise = myAnd bs

myConcat :: [[a]] -> [a]
myConcat [] = []
myConcat (xs : xxs) = xs ++ myConcat xxs

myReplicate :: Int -> a -> [a]
myReplicate 0 _ = []
myReplicate n a = a : myReplicate (n - 1) a

(!!!) :: [a] -> Int -> a
(x : xs) !!! n
    | n == 0 = x
    | otherwise = xs !!! (n - 1)

myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem a (x : xs)
    | a == x = True
    | otherwise = myElem a xs

-- 7
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge [] (y : ys) = y : merge [] ys
merge (x : xs) [] = x : merge xs []
merge (x : xs) (y : ys)
    | x < y = x : merge xs (y : ys)
    | otherwise = y : merge (x : xs) ys

-- 8
halve :: [a] -> ([a], [a])
halve xs = (take n xs, drop n xs)
  where
    n = length xs `div` 2

msort :: (Ord a) => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs =
    merge (msort a) (msort b)
  where
    (a, b) = halve xs

-- 9
mySum :: (Num a) => [a] -> a
mySum [] = 0
mySum (n : ns) = n + mySum ns

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake n (x : xs) = x : myTake (n - 1) xs

myLast :: [a] -> a
myLast [x] = x
myLast (_ : xs) = myLast xs
