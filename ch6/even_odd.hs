myEven :: Int -> Bool
myEven 0 = True
myEven n = myOdd (n - 1)

myOdd :: Int -> Bool
myOdd 0 = False
myOdd n = myEven (n - 1)

evens [] = []
evens (x : xs) = x : odds xs
odds [] = []
odds (_ : xs) = evens xs