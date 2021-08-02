-- Jack Yang, 30062393
{-- :                                                                                  EXERCISE 2: SOLUTION TEMPLATE

Instructions:
-----------------
1. Write your solutions to the exercise in this file. For each problem, a template for the solution is provided. Modify the templates to provide your solution. You can add extra functions to implement your solution. 

Please do not forget to document your code!

2. LEAVE the templates of the problems which you have not solved, unmodified. DO NOT ERASE the templates of the problems you have not solved. 

3. UPDATE THE "answered" LIST BELOW with the list of problems you have solved. i.e, if you implemented twoTautology function for q1, add 
    "twoTautology" to the "answered" list. The GradeScope autograder will test all the solutions whch is a member of this list and update your 
    grade. If you answer more than required number of questions, top scoring 6 answers will be considered for your grade. No marks will be 
    awarded for unsolved problems. 

4.  Do not change the name of this file.

We will be updating the autograder with new test cases and the assignments will be graded by the updated autograded after the submission 
due date. But, do make sure that your solutions passes the test cases of the autograder available at the time of submission. 

Good luck!
:-}

module Ex2Soln_old where   

data SF a = FF | SS a  
            deriving Show

-- IMPORTANT: List all the functions that you have implemented in the assignment. Your submission will be graded only based on the items in this list.
-- Uncomment the functions you have implemented
answered = [
   "twoTautology"  
--    "twoEquiv",
--    "badFermat",
--   "collatzIndex",
--   "bsort",
--    "qsort",
--     "msort",
--  "transpose",
--    "addMat",
--   "multMat",
--    "nreverse",
--   "freverse",
--   "hreverse",
--   "isAVL",
--   "fact",
--   "widthRose"
    ]


-- 1 
twoTautology :: ((Bool,Bool) -> Bool) -> Bool
-- Provide your answer below
twoTautology f = all f [(True, True), (False,True), (True,False), (False,False)]


twoEquiv :: ((Bool,Bool)->Bool)->((Bool,Bool)->Bool)->Bool
-- Provide your answer below
twoEquiv f g = f (True, True) == g (True, True) && f (False,True) == g (False,True) && f (True,False) == g (True,False) && f (False,False) == g (False,False)

--2
badFermat :: Integer
badFermat = badFermat' 1
    where
        badFermat' n = if isPrime (f' n) 2 then badFermat' (n+1) else n
        f' n = 2^2^n + 1
        
isPrime :: Float -> Integer -> Bool
isPrime n x
    | x > toInteger' (sqrt n) = True
    | (toInteger' n `mod` x) == 0 = False
    | x <= toInteger' (sqrt n) = isPrime n (x+1)
    | otherwise = False

toInteger' :: Float -> Integer
toInteger' = ceiling

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
bisection f ab
    | (f (fst ab) > 0) || (f (snd ab) < 0) = Nothing
    | f (uncurry (+) ab/2) < 0.0001 = Just $ uncurry (+) ab/2
    | f (uncurry (+) ab/2) > 0 = bisection f (fst ab, uncurry (+) ab/2)
    | otherwise = bisection f (uncurry (+) ab/2, snd ab)

-- 5
bsort:: Integral a => (a -> a -> Bool) -> [a] -> [a]
bsort _ _ = []        


qsort:: Integral a => (a -> a -> Bool) -> [a] -> [a]
qsort _ _  = []

msort:: Integral a =>  (a -> a -> Bool) -> [a] -> [a]
msort _ _  = []

-- 6
type Matrix a = [[a]] 
type DoubleMatrix = Matrix Double

transpose:: Matrix a -> (Maybe (Matrix a))
transpose _ = Nothing

addMat :: DoubleMatrix -> DoubleMatrix -> (Maybe DoubleMatrix)
addMat _ _  = Nothing

multMat :: DoubleMatrix -> DoubleMatrix -> (Maybe DoubleMatrix)
multMat _ _ = Nothing

-- 7
nreverse :: (Ord a, Integral a) => [a] -> [a]
nreverse _ = []

freverse :: (Ord a, Integral a) => [a] -> [a]
freverse _ = []


hreverse :: (Ord a, Integral a) => [a] -> [a]
hreverse  _ =  []

-- 8
data STree a = Node (STree a) a (STree a) | Leaf

isAVL:: (Ord a, Integral a) => STree a -> Bool
isAVL _ = True

-- 9
fact _ = 0

-- 10
data Rose a = RS a [Rose a] deriving Show

rTree = RS 1 [RS 0 [], RS 2 [RS 3 [], RS 4 [RS 5 []], RS 9 []], RS 6 [RS 7 [], RS 8 []]]

widthRose :: Integral a =>  Rose a -> Int
widthRose _ = 0