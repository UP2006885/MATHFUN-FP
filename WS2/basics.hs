absolute :: Int -> Int
absolute x
    | x >= 0    = x
    | otherwise = x * (-1)

sign :: Int -> Int
sign x
    | x == 0    = 0
    | x > 0     = 1
    | otherwise = (-1)

howManyEqual :: Int -> Int -> Int -> Int
howManyEqual x y z
    | x == y && y == z && z == x = 3
    | x == y || y == z || z == x = 2
    | otherwise = 0

sumDiagonalLengths :: Float -> Float -> Float -> Float
sumDiagonalLengths x y z = ((x*rootTwo) + (y*rootTwo) + (z*rootTwo))
    where 
    rootTwo = 1.41421356237

taxiFair :: Int -> Float
taxiFair x
    | x < 1 = defaultPrice
    | x < 11 = defaultPrice + (fromIntegral(x) * first10Kms)
    | otherwise = defaultPrice + (5) + ((fromIntegral(x) - 10) * additionalKms)
    where 
    defaultPrice = 2.20
    first10Kms = 0.50
    additionalKms = 0.30

howManyAboveAverage :: Int -> Int -> Int -> Int
howManyAboveAverage x y z
    | (x > average && y > average && z > average) = 3
    | ((x > average && y > average) || (z > average && y > average) || (z > average && x > average)) = 2
    | (x > average || y > average || z > average) = 1
    |  otherwise = 0
    where
    average = round(fromIntegral(x+y+z)/3)

validDate :: Int -> Int -> Bool
validDate x y
    | ((x < 1 || y < 1) || (x > 31 || y > 12)) = False
    | (y == 2 && x > 28) = False
    | ((y == 4 || y ==  6 || y == 9 || y == 11) && x > 30) = False
    | otherwise = True

daysInMonth :: Int -> Int -> Int
daysInMonth x y
    | (x > 12 || x < 1) = 0
    | ((y `mod` 4) == 0 && x == 2) = 29
    | x == 2 = 28
    | x == 4 || x == 6 || x == 9 || x == 11 = 30
    | otherwise = 31