myOdd n = not (even n)

twice f x = f (f x)

sumsqreven ns = sum (map (^ 2) (filter even ns))

myOdd2 = not . even

twice2 f = f . f

sumsqreven2 = sum . map (^ 2) . filter even

compose :: [a -> a] -> (a -> a)
compose = foldr (.) id