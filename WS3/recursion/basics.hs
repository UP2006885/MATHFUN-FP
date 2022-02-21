fact :: Int -> Int
fact n
    | n > 0 = n * fact (n - 1)
    | n == 0 = 1
    | otherwise = error "undefined for neg ints"

multi :: Int -> Int -> Int
multi n x
    | n == 0    = 0
    | n > 0     = x + multi (n - 1) x

divide :: Int -> Int -> Int
divide n m
    | n < m = 0
    | otherwise = 1 + divide (n - m) m

    