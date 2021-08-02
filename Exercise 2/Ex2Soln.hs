-- Jack Yang, 30062393
{-- :                                                                                  EXERCISE 2: SOLUTION TEMPLATE

Instructions:
-----------------
1. Write your solutions to the exercise in this file. For each problem, a template for the solution is provided. Modify the templates
    to provide your solution. You can add extra  functions to implement your solution. 

2. LEAVE the templates of the problems which you have not solved, unmodified. DO NOT ERASE the templates of the problems you 
    have not solved. 

3. GradeScope will test all the questions from the template each time. Top scoring 6 answers will be considered for your grade. No marks will be 
    awarded for unsolved problems. 

We will be updating the autograder with new test cases and the assignments will be graded by the updated autograded after the submission 
due date. But, do make sure that your solutions passes the test cases of the autograder available at the time of submission. 

Good luck!
:-}

module Submission where

import Prelude hiding (maybe, flip, curry, zipWith, foldr, filter, splitAt, length, (++), foldl, take, const)


data SF a = FF | SS a  
            deriving (Eq, Show)

-- Tests all possible combinations for twoTautology
-- 1 
twoTautology :: ((Bool,Bool) -> Bool) -> Bool
-- Provide your answer below
twoTautology f = all f [(True, True), (False,True), (True,False), (False,False)]

-- Tests all possible combinations for twoEquiv
twoEquiv :: ((Bool,Bool)->Bool)->((Bool,Bool)->Bool)->Bool
-- Provide your answer below
twoEquiv f g = f (True, True) == g (True, True) && f (False,True) == g (False,True) && f (True,False) == g (True,False) && f (False,False) == g (False,False)

-- Uses the equation and functiono isPrime to find a badFermat number that is not prime
--2
badFermat :: Integer
badFermat = badFermat' 1
    where
        badFermat' n = if isPrime (f' n) 2 then badFermat' (n+1) else n
        f' n = 2^2^n + 1

-- If there exists a number that can be divided equally up to the sqrt of x then it's not a prime, otherwise it is
isPrime :: Float -> Integer -> Bool
isPrime n x
    | x > toInteger' (sqrt n) = True
    | (toInteger' n `mod` x) == 0 = False
    | x <= toInteger' (sqrt n) = isPrime n (x+1)
    | otherwise = False

-- Converts a float to an integer
toInteger' :: Float -> Integer
toInteger' = ceiling


-- If it's even then do collatzEven, if it's odd then do collatzOdd. Recursively accumulate to list.
-- 3 
collatzIndex ::  Int -> SF [Int]
-- Provide your answer below
collatzIndex n = if n <= 0 then FF else SS $ collatzIndex' n

collatzIndex' :: Int -> [Int]
collatzIndex' n
    | n == 1 = [1]
    | even n = n : collatzEven n
    | odd n = n : collatzOdd n 
        where
            collatzEven n = collatzIndex' (div n 2)
            collatzOdd n = collatzIndex' ((3*n) + 1)

-- 4
e :: Double
e = exp (-150)

bisection::(Double->Double)->(Double,Double)->Maybe Double
-- Provide your answer below
bisection f (a, b)
    | (f a * f b) > 0 = Nothing
    | f m < 0.001 || (a-m) < 0.001 = Just m
    | f m > 0 && f a < 0 = bisection f (a, m)
    | f m > 0 && f b < 0 = bisection f (b, m)
    | f m < 0 && f a > 0 = bisection f (m, a)
    | f m < 0 && f b > 0 = bisection f (m, b)
    where  
        m = (a+b)/2

-- Does bubblesort by swapping pairs of elements if not in correct order
-- 5
bsort:: Integral a => (a -> a -> Bool) -> [a] -> [a]
bsort f list  = if sorted list then list else bubbleSort list
    where
        sorted [] = True
        sorted [x] = True 
        sorted [x,y] = f x y
        sorted (x:y:xs) = f x y && sorted (y:xs)
        bubbleSort [x,y] = if f x y then [x,y] else [y,x]
        bubbleSort (x:y:xs) = if f x y then bsort f $ x : bubbleSort (y:xs) else bsort f $ swap (x:y:xs)
        swap (x:y:xs) = y : bubbleSort (x:xs)       


-- Does quicksort by splitting the list in half and gathering all the correct elements either on the left or right of chosen element
qsort:: Integral a => (a -> a -> Bool) -> [a] -> [a]
qsort f [] = []
qsort f (x:xs) = leftList x ++ [x] ++ rightList x
    where
        leftList m = qsort f [n | n <- xs, f n m]
        rightList m = qsort f [n | n <- xs, not (f n m)]

(++) :: [a] -> [a] -> [a]
(++) [] ys = ys
(++) (x:xs) ys = x : (++) xs ys

-- Does merge sort by splitting list then merging the split lists in order.
msort:: Integral a =>  (a -> a -> Bool) -> [a] -> [a]
msort f [] = []
msort f [x] = [x]
msort f list = merge half1 half2
    where
        merge [] y = y
        merge x [] = x
        merge (x:xs) (y:ys) = if f x y then x : merge xs (y:ys) else y : merge (x:xs) ys
        half1 = msort f (take (length list `div` 2) list)
        half2 = msort f (drop (length list `div` 2) list)

-- Take a certain number of elements in a list
take :: (Eq t, Num t) => t -> [a] -> [a]
take n (x:xs)
    | n == 0 = []
    | otherwise = x : take (n-1) xs

-- Calculate length of list
length :: [a] -> Int
length [] = 0
length [x] = 1
length (x:xs) = length xs + 1


-- 6
type Matrix a = [[a]] 
type DoubleMatrix = Matrix Double

{-
[[1,3,4]
 [2,3,2]]

[[1,2]
 [3,3]
 [4,2]]
-}
-- take first of every list, then second, ...
-- if elements of list aren't of equal size then Nothing
transpose:: Matrix a -> (Maybe (Matrix a))
transpose matrix = if equal matrix then Just (transpose' matrix) else Nothing 
    where
        transpose' ([]:_) = []
        transpose' matrix = map head matrix : transpose' (map tail matrix)
        equal matrix = all ((==length (head matrix)) . length) matrix

{-
[[1,3]    [[3,4]
 [2,3]] +  [5,4]]
=
[[4,7]
 [7,7]]
-}
-- add elements using zipWith for every sublist
-- the 2 matrices need to be of identical size and shape
addMat :: DoubleMatrix -> DoubleMatrix -> (Maybe DoubleMatrix)
addMat matrix1 matrix2 = if matrix1 /= [] && matrix2 /= [] && equal matrix1 matrix2 then Just (addMat' matrix1 matrix2) else Nothing 
    where
        addMat' [] [] = []
        addMat' matrix1 matrix2 = zipWith (+) (head matrix1) (head matrix2) : addMat' (tail matrix1) (tail matrix2)
        equal matrix1 matrix2 = ((length (head matrix1) * length matrix1) == (length (head matrix2) * length matrix2)) && (length matrix1 == length matrix2)

zipWith f [] [] = []
zipWith f [] x = x
zipWith f x [] = x
zipWith f l1 l2 = f (head l1) (head l2) : zipWith f (tail l1) (tail l2)

{-
[[8,9]]   [[3],
        *  [5],
           [7]]
=
[[69]]
-}
-- Start by transposing the second matrix so its easier to use zipWith to get the initial elements before adding them together.
-- then map the sum to get the multiplied element
-- repeat for every sublist in the first matrix
multMat :: DoubleMatrix -> DoubleMatrix -> (Maybe DoubleMatrix)
multMat matrix1 matrix2
    | null matrix1 && null matrix2 = Just [] 
    | matrix1 /= [] && matrix2 /= [] && equal matrix1 matrix2 = Just (multMat' matrix1 (transpose' matrix2)) 
    | otherwise = Nothing 
        where
            multMat' [] x = []
            multMat' matrix1 matrix2 = map (sum . zipWith (*) (head matrix1)) matrix2 : multMat' (tail matrix1) matrix2
            equal matrix1 matrix2 = (length (head matrix1) == length matrix2) && all ((== head (map length matrix1)) . length) matrix1 && all ((== head (map length matrix2)) . length) matrix2

-- transpose without the Maybe 
transpose':: Matrix a -> Matrix a
transpose' matrix = transpose' matrix
    where
        transpose' ([]:_) = []
        transpose' matrix = map head matrix : transpose' (map tail matrix)

-- 7
-- naive reverse using append
nreverse :: (Ord a, Integral a) => [a] -> [a]
nreverse [] = []
nreverse [x] = [x]
nreverse (x:xs) = nreverse xs ++ [x]

-- fast reverse using an accumulator
freverse :: (Ord a, Integral a) => [a] -> [a]
freverse [] = []
freverse list = freverse' list []
    where
        freverse' [x] y = x : y
        freverse' (x:xs) y = freverse' xs (x : y)

hreverse :: (Ord a, Integral a) => [a] -> [a]
hreverse  _ =  []

-- 8
data STree a = Node (STree a) a (STree a) | Leaf  deriving (Show)

isAVL:: (Ord a, Integral a) => STree a -> Bool
isAVL _ = True

-- 9
-- 1 * 2 * 3 * ... * n
fact :: Integer
fact = product [1..1891]

-- 10
data Rose a = RS a [Rose a]   deriving (Show)

rTree = RS 1 [RS 0 [], RS 2 [RS 3 [], RS 4 [RS 5 []], RS 9 []], RS 6 [RS 7 [], RS 8 []]]

widthRose :: Integral a =>  Rose a -> Int
widthRose _ = 0
