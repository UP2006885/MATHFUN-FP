absolute :: Int -> Int
absolute x
    | x >= 0    = x
    | otherwise = x * (-1)

sign :: Int -> Int -- 1 = P 0 - -1 N 0 - 0
sign x
    | x == 0    = 0
    | x > 0     = 1
    | otherwise = (-1)

howManyFunctions :: Int -> Int -> Int -> Int
howManyFunctions x y z
    | x == y && y == z && z == x = 3
    | x == y || y == z || z == x = 2
    | otherwise = 0

-- Come back to this one.
-- sumDiagonalLengths :: Float -> Float -> Float -> Float
-- sumDiagonalLengths x y z

-- 2.20
-- 50p Per Kil for first 10
-- 30p Per Kil for additional.
-- Guards
gTaxiFairs :: Float -> Float
gTaxiFairs x
    | x == 1 = 2.20
    | x > 1 && x < 11 = (2.20 + ((x-1) * 0.50))
    | otherwise = (2.20 + (9 * 0.50) + ((x - 10) * 0.30))

-- Where
-- taxiFairs :: Int -> Float