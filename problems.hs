-- 99 problems but Haskell aint one

import Data.Either -- for list encoder

--- 1 through 10: Lists --- 

-- 1:
-- Find the last element of a list.

myLast :: [a] -> a
myLast = foldl1 (\_ x -> x)
--or
myLast' :: [a] -> a
myLast' = last 

-- 2:
-- Find the second to last element of a list.

myButLast :: [a] -> a
myButLast [] = error "Empty set, you idiot. You fucking moron"
myButLast (x:[]) = error "no"
myButLast (x:xs)
  | length xs == 2 = head xs
  | otherwise = myButLast xs
-- or
myButLast' :: [a] -> a
myButLast' = last . init

-- 3:
-- Find the K'th element of a list, where the first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt xs n = xs !! (n-1)
-- or
elementAt' :: [a] -> Int -> a
elementAt' xs n = last $ (take n) xs

-- 4:
-- Find the number of elements of a list

myLength :: [a] -> Int
myLength = foldl (\acc _ -> acc+1) 0
-- or
myLength' :: [a] -> Int
myLength' xs = sum [1 | _ <- xs]
-- or
myLength'' :: [a] -> Int 
myLength'' [] = 0
myLength'' (_:xs) = 1 + myLength'' xs
-- or
myLength3 :: [a] -> Int
myLength3 xs  = sum $ zipWith (\y _ -> y) (repeat 1) xs
-- or even (this is my favorite)
myLength4 :: [a] -> Int
myLength4 = fst . last . zip [1..]

-- 5:
-- Reverse a list

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 6:
-- Find out whether a list is a palindrome

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome [] = True
isPalindrome xs = null [x | x <- xs `zip` myReverse xs, fst x /= snd x]
-- or
isPalindrome' :: (Eq a) => [a] -> Bool
isPalindrome' [] = True
isPalindrome' xs = xs == (reverse xs)

-- 7:
-- Flatten a nested list structure

-- Must define a new data type; lists in Haskell are homogeneous:

data NestedList a = Elem a | List [NestedList a] deriving (Show)

-- now we undo it

myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List (x:xs)) = myFlatten x ++ myFlatten (List xs)
myFlatten (List []) = []

-- 8:
-- Eliminate consecutive duplicates of list elements

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x:xs)
  | x == head xs = compress xs
  | otherwise    = [x] ++ compress xs

-- or

compress2 :: Eq a => [a] -> [a]
compress2 [] = []
compress2 (x:xs) = x : (compress2 $ dropWhile (== x) xs)

-- 9:
-- Pack consecutive duplicates of list elements into sublists.

pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = ([x] ++ takeWhile (==x) xs):(pack $ dropWhile (==x) xs)

-- 10:
-- Run-length encoding of a list.

encode :: Eq a => [a] -> [(Int,a)]
encode [] = []
encode xs = map (\ys -> (length ys, head ys)) $ pack xs

--- 11 through 20: Lists, continued

-- 11:
-- Modified run-length encoding.

encodeMod :: Eq a => [a] -> [Either a (Int, a)]
encodeMod [] = []
encodeMod xs = map (\ys -> if length ys == 1 then Left (head ys) else Right (length ys, head ys)) $ pack xs

-- 12:
-- Decode modified run-length encoding.

decodeMod :: Eq a => [Either a (Int, a)] -> [a]
decodeMod [] = []
decodeMod (x:xs) = toList x ++ decodeMod xs
  where toList (Left y) = [y]
        toList (Right (n, y)) = replicate n y

-- or, with concatMap

decodeMod2 :: Eq a => [Either a (Int, a)] -> [a]
decodeMod2  = concatMap toList 
  where toList (Left y) = [y]
        toList (Right (n, y)) = replicate n y
  
-- 13:
-- Run-length encoding of a list (direct solution)
-- that is, encode the list without first constructing sublists

-- UNDER CONSTRUCTION

-- 14:
-- Duplicate the elements of a list
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = [x,x] ++ dupli xs

-- 15:
-- Replicate the elements of a list a given number of times

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (replicate n x) ++ (repli xs n)

-- 16:
-- Drop every Nth element from a list

dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n
  | length xs < n = xs
  | otherwise     = [fst x | x <- zipList, snd x `mod` n /= 0] 
  where zipList = zip xs [1..]

-- 17:
-- Split a list into two parts, the length of the first part
-- is given. Do not use any predefined predicates.

split :: [a] -> Int -> ([a],[a])
split xs n
  | n >= l    = (xs,[])
  |otherwise = ([xs!!(i-1) | i<-[1..n]], [xs!!(i-1) | i<-[(n+1)..l]])
  where
    l = length xs
  
-- 18: 
-- Extract a slice from a list

-- UNDER CONSTRUCTION. kind of boring tbh

-- 19:
-- Rotate a list N places to the left

rotate :: [a] -> Int -> [a]
rotate [] _ = []
rotate xs 0 = xs
rotate xs n 
  | n > 0 = drop nn xs ++ take nn xs
  | n < 0 = drop (l+nn) xs ++ take (l+nn) xs
  where l = length xs
        nn = n `mod` l 
    

--- 31 through 41: Arithmetic ---

-- 31:
-- Determine whether a given integer number is prime.

-- this way is probably dumb
-- but it seems to work
isPrime' :: Int -> Bool
isPrime' 0 = False
isPrime' 1 = False
isPrime' 2 = True
isPrime' n = null [x | x <- [2 .. lim], n `mod` x == 0]
  where lim = ceiling . sqrt . fromIntegral $ n


-- remove composite numbers from a list of integers
-- mostly for use in sieve below
removeComposites :: [Int] -> [Int]
removeComposites [] = []
removeComposites (x:xs) = [x] ++ removeComposites [y | y <- xs, y `mod` x /= 0] 

-- Sieve of Eratosthenes
primeSieve :: Int -> [Int]
primeSieve n
  | n < 2 = error "No primes less than 2."
  | n >= 2 = removeComposites [2..n]

--getLim = primeSieve . ceiling . sqrt . fromIntegral

isPrime :: Int -> Bool
isPrime p = null [x | x <- (getLim p), p `mod` x == 0]
  where getLim = primeSieve . ceiling . sqrt . fromIntegral
  


