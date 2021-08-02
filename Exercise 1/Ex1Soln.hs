-- Jack Yang, 30062393
{-- :                                                                                  EXERCISE 1: SOLUTION TEMPLATE

Instructions:
-----------------
1. Write your solutions to the exercise in this file. For each problem, a template for the solution is provided. Modify the templates to provide your solution. You can add extra functions to implement your solution. 

Please do not forget to add appropriate comments to you code!

2. LEAVE the templates of the problems which you have not solved, unmodified. DO NOT ERASE the templates of the problems you 
    have not solved. 

3. UPDATE THE "answered" LIST BELOW with the list of problems you have solved. i.e, if you implemented twoTautology function for q1, add 
    "twoTautology" to the "answered" list. The GradeScope autograder will test all the solutions whch is a member of this list and update your 
    grade. If you answer more than required number of questions, top scoring 6 answers will be considered for your grade. No marks will be 
    awarded for unsolved problems. 

4.  Do not change the name of this file.

We will be updating the autograder with new test cases and the assignments will be graded by the updated autograded after the submission 
due date. But, do make sure that your solutions passes the test cases of the autograder available at the time of submission. 

Good luck!
:-}

module Ex1Soln where   

data SF a = FF | SS a  
            deriving Show

data List a = Node a (List a) | Null
            deriving Show 

-- IMPORTANT: List all the functions that you have implemented in the assignment. Your submission will be graded only based on the items in this list.
-- Uncomment the functions you have implemented
answered = [
   "avgThree",
    "maxThree",
--    "invFac",
   "myGcd",
   "binom",
   "grow",
    "instrictorder",
  "cheapItems",
    "sortByCost",
   "divisors",
    "substring",
   "sublists"
    ]


-- 1 
-- Adds three floats from integers, then divides by three to get average
avgThree:: Int -> Int -> Int -> Float
-- Provide your answer below
avgThree x y z = (fromIntegral x + fromIntegral y + fromIntegral z) / 3.0

--2
-- if x == y == z then all three are same
-- otherwise if one pair is same then its 2 that are same
-- otherwise otherwise none are same
maxThree:: Int -> Int -> Int -> (Int,Int)
-- Provide your answer below
maxThree x y z
    | (x==y) && (y==z) = (max, 3)
    | (x == y) || (x == z) || (z == y) = (max, 2)
    | otherwise = (max, 1)
    where max
            | (x >= y) && (x >= z) = x
            | (y >= x) && (y >= z) = y
            | (z >= y) && (z >= x) = z

-- 3
-- start by testing if x factorial is LE to x, if not test x-1, repeat
invFac:: Integer -> SF Integer
invFac x = if x < 0 then FF else SS $ myHead [n | n <- [x, x-1..0], currFac n <= x]
    where
        currFac 0 = 1
        currFac x = x * currFac (x - 1)
    
myHead:: [Integer] -> Integer 
myHead [] = 1
myHead (x:xs) = x

-- 4
-- if either x or y is 0 then gcd = the other int
-- otherwise recursively find gcd using euclideans algorithm
myGcd :: Int -> Int -> Int
myGcd x y 
    | x == 0 = abs y
    | y == 0 = abs x
    | abs x > abs y = myGcd (mod (abs x) (abs y)) (abs y)
    | abs y > abs x = myGcd (abs x) (mod (abs y) (abs x))

-- 5
-- uses the equation from exercise1 sheet to find binom output
binom:: Integer -> Integer -> Integer
binom x y = myProduct [x,x-1..x-y+1] `div` myProduct [1..y]
    where
        myProduct list = foldr (*) 1 list

-- 6
-- recursively take each letter and replicate it by y many times starting at 1 in groww, then append it to the rest of the string
grow :: String -> String
grow x = groww x 1
    where
        groww [] y = ""
        groww (x:xs) y = myReplicate y x `myAppend` groww xs (y+1) 
        myReplicate a z = myTake a (myRepeat z)
        myRepeat x = xs where xs = x:xs

-- from hoogle
myTake :: (Eq t, Num t) => t -> [a] -> [a]
myTake 1 (x:_) = [x]
myTake n (x:xs) = x : myTake (n - 1) xs

-- grabbed from lecture
myAppend :: [a] -> [a] -> [a]
myAppend [] ys = ys
myAppend (x:xs) ys = x : myAppend xs ys

-- 7
-- iterate through the list recursively and if a pair is found to be decreasing, then return false
instrictorder:: [Int]-> Bool
instrictorder [] = True
instrictorder [x] = True 
instrictorder [x, y] =  x < y
instrictorder (x:y:xs) = x < y && instrictorder (y:xs)
 
-- 8
-- go through each tuple in list and if the price Int is evaluated to be cheaper than given Int, then add to list returned
cheapItems:: [(String,Int)] -> Int -> [String]
cheapItems list y = [myFst x | x <- list, mySnd x < y]

myFst :: (a, b) -> a
myFst (x,_) = x

mySnd :: (a, b) -> b
mySnd (_,y) = y

-- 9
-- sorts list by ussing bubble sort
-- first check to see if list is sorted, if not bubble sort the list, checks if list is sorted every iteration of bubble sort
sortByCost :: [(String,Int)] -> [(String,Int)]
sortByCost list = if sorted list then list else bubbleSort list
    where
        sorted [] = True
        sorted [x] = True 
        sorted [x,y] = mySnd x <= mySnd y
        sorted (x:y:xs) = (mySnd x <= mySnd y) && sorted (y:xs)
        bubbleSort [x,y] = if mySnd x > mySnd y then [y,x] else [x,y]
        bubbleSort (x:y:xs) = if mySnd x > mySnd y then sortByCost $ swap (x:y:xs) else sortByCost $ x : bubbleSort (y:xs)
        swap (x:y:xs) = y : bubbleSort (x:xs)

-- 10
-- if x LE to 1 then return empty list
-- otherwise first find list of all factors, then using filter, filter out the primes by determining if the number has any factors (if not its a prime).
divisors:: Integer -> [Integer]
divisors x
    | x <= 1 = []
    | otherwise = myFilter prime (factors x)
    where 
        factors y = [n | n <- [2..y-1], y `mod` n == 0]
        prime z = null (factors z) 

-- filter grabbed from lecture
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p [] = []
myFilter p (x:xs)
    | p x = x:myFilter p xs
    | otherwise = myFilter p xs

-- 11
-- finds checks str1 one char at a time and returns true if all are found in str2 in sequence.
substring :: String -> String -> Bool
substring str1 str2
    | length str1 > length str2 = False
    | length str1 == 1 = myHead' str1 `elem` str2
    | otherwise = substring' str1 str1 str2
    where
        substring' str1 [x] [y] = x == y
        substring' str1 (x:xs) [y] = False
        substring' str1 [x] (y:ys) = x == y || substring' str1 str1 (y:ys)
        substring' str1 (x:xs) (y:ys) = if x == y then substring' str1 xs ys else substring' str1 str1 ys

myHead' :: String  -> Char 
myHead' [] = ' '
myHead' (x:xs) = x

myLength :: Num a1 => [a2] -> a1
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 12
-- grabbed from tutorial
sublists:: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) = [x:s | s<-sublists xs] `myAppend` sublists xs