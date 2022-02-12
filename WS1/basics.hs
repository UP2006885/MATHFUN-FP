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