module Run where

import Prelude hiding (maybe, flip, curry, zipWith, foldr, filter, splitAt, length, (++), foldl, take, const, takeWhile)

data SF a = SS a | FF
    deriving (Show, Eq)

myZip [] _ = []
myZip _ [] = []
myZip (a:as) (b:bs) = (a,b):(myZip as bs)

grow :: String -> String
grow (x:xs) = grow (x : [x | (x,y) <- myZip xs xs, x == y])

myhead :: [a] -> SF a
myhead [] = FF
myhead (a:as) = SS a

mytail :: [a] -> [a]
mytail [] = []
mytail (_:xs) = xs

myster :: Eq a => [a] -> SF a
myster xs = myhead [x | (x,y) <- myZip xs (mytail xs), x == y]

-----------------------------------------------------------------------------------------------------
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b [] = b
foldr f b (x:xs) = f x (foldr f b xs)

myOR :: [Bool] -> Bool
myOR = foldr myor False

myor :: Bool -> Bool -> Bool
myor False False = False 
myor _ _ = True


same x y = if x == y then x else []

dooo b f y = b

-- get all repeating consecutive elements in list
--mystery [x] = FF
--mystery (x:y:xs) = if x == y then SS x else mystery (y:xs)

mystery [] = FF
mystery (x:xs) = if length (takeWhile (==x) xs) > 0 then myhead (takeWhile (==x) xs) else mystery xs
    where
        takeWhile f = foldr (\x xs -> if f x then x:xs else []) []
        length [] = 0
        length (_:xs) = 1 + length xs




filter p [] = []
filter p (a:as)
    | p a = a : filter p as
    | otherwise = filter p as

p a = a == 3