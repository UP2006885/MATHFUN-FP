-- function to return the absolute value of the first element of a list, or -1 for an empty list.
absFirst :: [Int] -> Int
absFirst [] = -1
absFirst (x:xs) = abs x

-- Sum function to return the sum of a list of integers.
newSum :: [Int] -> Int
newSum [] = 0
newSum (x:xs) = x + sum xs