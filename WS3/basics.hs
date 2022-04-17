-- We don't import '||' from the prelude, so that we can 
-- define our own version

import Prelude hiding ((||), (&&), gcd) 

-- The following line declares the || operator (which we are about to
-- re-define) to be right associative and to have precedence 2. This
-- is necessary in order for expressions such as False || x > 2 to be
-- valid (e.g. it sets the precedence of || to be lower than >). 

infixr 2  ||
infixr 3  &&


-- A naive re-implementation of the Prelude operator ||
(||) :: Bool -> Bool -> Bool
True || True    = True
False || True   = True
True || False   = True
False || False  = False

-- An alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--False || False   = False
--_ || _           = True

-- Another alternative re-implementation
--(||) :: Bool -> Bool -> Bool
--True || _     =  True
--False || a    = a


fact :: Int -> Int 
fact n 
    | n == 0    = 1
    | n > 0     = n * fact (n - 1)
    | otherwise = error "factorials not defined for negative ints"

mult :: Int -> Int -> Int
mult n m 
    | n == 0        = 0
    | n > 0         = m + mult (n - 1) m 

divide :: Int -> Int -> Int
divide n m
    | n < m         = 0
    | otherwise     = 1 + divide (n - m) m


-- My code.

-- (&&) :: Bool -> Bool -> Bool
-- True && True    = True
-- True && False   = False
-- False && True   = False
-- False && False  = False

-- (&&) :: Bool -> Bool -> Bool
-- True && True    = True
-- _ && _          = False

(&&) :: Bool -> Bool -> Bool
True && True    = True
True && b          = b
c && True          = c

exOr :: Bool -> Bool -> Bool
exOr False b    = b
exOr a False    = a
exOr _  _        = False

ifThenElse :: Bool -> Int -> Int -> Int
ifThenElse True x y    = x
ifThenElse False x y   = y

daysInMonth :: Int -> Int
daysInMonth 2 = 28
daysInMonth 4 = 20
daysInMonth 6 = 20
daysInMonth 9 = 20
daysInMonth 11 = 20
daysInMonth m = if (m > 12 || m < 0) then 0 else 31

validDateChecker:: Int -> Bool
validDateChecker m = if daysInMonth m == 0 then False else True

-- Recursion

sumNumbers :: Int -> Int
sumNumbers n
    | n == 0    = 0
    | n > 0     = sumNumbers(n - 1) + n

sumSquares :: Int -> Int
sumSquares n
    | n == 0 = 0
    | n > 0 = sumSquares(n - 1) + n^2

power :: Int -> Int -> Int
power z x
    | x == 0 = 1
    | x > 0 = z * power z (x-1)

sumFromTo :: Int -> Int -> Int
sumFromTo x y
    | x > y = 0
    | x <= y = sumFromTo(x+1) y + x

gcd :: Int -> Int -> Int
gcd x y
    | y == 0 = abs x
    | otherwise = gcd y (mod x y)

intSquareRoot :: Int -> Int
intSquareRoot n = findRoot n n

-- Find Root Missing.