timesTen :: Int -> Int
timesTen x = 10 * x

sumThree :: Int -> Int -> Int -> Int
sumThree a b c = a + b + c

areaOfCircle :: Float -> Float
areaOfCircle x = pi * (x ^ 2)

volumeOfCylinder :: Float -> Float -> Float
volumeOfCylinder r l = (areaOfCircle r) * l

distance :: Float -> Float -> Float -> Float -> Float
distance x1 y1 x2 y2 = sqrt(((y1 - y2)^2) + ((x1 - x2)^2))

threeDifference :: Int -> Int -> Int -> Bool
threeDifference x y z = if (x /= y && x /= z && z /= y) then True else False

divisibleBy :: Int -> Int -> Bool
divisibleBy x y = if((x `mod` y) == 0) then True else False

isEven :: Int -> Bool
isEven x = (divisibleBy x 2)

averageOfThree :: Int -> Int -> Int -> Float
averageOfThree x y z = (fromIntegral(x+y+z)/3)

absolute :: Int -> Int
absolute x = if(x > 0) then x else (x * (-1))