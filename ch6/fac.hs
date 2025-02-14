fac1 n = product [1 .. n]

fac2 :: Int -> Int
fac2 0 = 1
fac2 n = n * fac2 (n - 1)