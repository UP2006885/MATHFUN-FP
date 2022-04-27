import Data.Char

type StudentMark = (String, Int)

betterStudent :: StudentMark -> StudentMark -> String
betterStudent (s1,m1) (s2,m2) 
    | m1 >= m2          = s1
    | otherwise         = s2

marks:: [StudentMark] -> [Int]
marks stmks = [ mk | (st,mk) <- stmks ]

pass :: [StudentMark] -> [String]
pass stmks = [ st | (st,mk) <- stmks, mk >= 40 ]

-- An example list of student marks
testData :: [StudentMark]
testData = [("John", 53), ("Sam", 16), ("Kate", 85), ("Jill", 65),
            ("Bill", 37), ("Amy", 22), ("Jack", 41), ("Sue", 71)]

addPairs :: [(Int,Int)] -> [Int]
addPairs pairList = [ i+j | (i,j) <- pairList ]

minAndMax :: Int -> Int -> (Int,Int)
minAndMax x y 
    | x <= y            = (x,y)
    | otherwise         = (y,x)


-- My Code.

-- Tuples
sumDifference :: Int -> Int -> (Int,Int)
sumDifference x y = (x+y, x-y)

grade :: StudentMark -> Char
grade (s1, m1)
    | m1 >= 70 = ('A')
    | m1 >= 60 = ('B')
    | m1 >= 50 = ('C')
    | m1 >= 40 = ('D')
    | otherwise = ('F')

grading :: StudentMark -> (String, Char)
grading (s1, m1)
    | m1 >= 70 = (s1, 'A')
    | m1 >= 60 = (s1, 'B')
    | m1 >= 50 = (s1, 'C')
    | m1 >= 40 = (s1, 'D')
    | otherwise = (s1, 'F')

capMark :: StudentMark -> StudentMark
capMark (s1, m1)
    | m1 > 40 = (s1, 40)
    | otherwise = (s1, m1)

-- Lists and Strings
firstNumbers :: Int -> [Int]
firstNumbers n 
    | n == 1 = [n]
    | otherwise = firstNumbers(n-1) ++ list
    where
    list = [n]

firstSquares :: Int -> [Int]
firstSquares n
    | n == 0 = []
    | otherwise = firstSquares(n-1) ++ list
    where
    list = [n^2]

--      List Comprehensions
capitalise :: String -> String
capitalise string = [ toUpper s | s <- string ]

onlyDigits :: String -> String
onlyDigits string = [ s | s <- string, not $ isAlpha s ]

capMarks :: [StudentMark] -> [StudentMark]
capMarks students = [capMark student | student <- students]

gradeStudents :: [StudentMark] -> [(String,Char)]
gradeStudents students = [grading m1 | m1 <- students]

-- duplicate ICBA

divisors :: Int -> [Int]
divisors n = [ x | x <- [1..(n-1)], n `rem` x == 0]

isPrime :: Int -> Bool
isPrime x
    | length (divisors(x)) <= 2 = True
    | otherwise = False

split:: [(a,b)] -> ([a],[b])
split listOfPairs = ([a | (a, _) <- listOfPairs], [b | (_, b) <- listOfPairs])