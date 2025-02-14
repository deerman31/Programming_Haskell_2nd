qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x : xs) = qsort small ++ [x] ++ qsort big
  where
    small = [a | a <- xs, a <= x]
    big = [b | b <- xs, b > x]
