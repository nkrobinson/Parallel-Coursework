---------------------------------------------------------------------------
-- Sequential Euler Totient Function
---------------------------------------------------------------------------
-- This program calculates the sum of the totients between a lower and an
-- upper limit, using arbitrary precision integers.
-- Phil Trinder, 26/6/03
-- Based on earlier work by Nathan Charles, Hans-Wolfgang Loidl and
-- Colin Runciman
---------------------------------------------------------------------------

module Main(main) where

import System.Environment
import System.IO
import System.CPUTime
import Text.Printf
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq

---------------------------------------------------------------------------

-- nicParMap :: (a -> a) -> [a] -> Int -> [a]
-- nicParMap f (x:xs) n
--         | (x:xs)==[] = []
--         | x < (n)  = (f x):(nicParMap f xs)
--         | otherwise  = (fn:fn2) `using` strat
--                     where fn = f x
--                           fn2 = nicParMap f xs
--                           strat result = do
--                             rpar fn2
--                             return result

---------------------------------------------------------------------------
-- Main Function, sumTotient
---------------------------------------------------------------------------
-- The main function, sumTotient
-- 1. Generates a list of integers between lower and upper
-- 2. Applies Euler's phi function to every element of the list
-- 3. Returns the sum of the results

sumTotient :: Int -> Int -> Int
sumTotient lower upper = sum (totient [lower, lower+1 .. upper])
totient :: [Int] -> [Int]
totient x = map euler x `using` parListChunk 100 rpar

-- sumTotient lower upper = divideSumTotient lower upper

divideSumTotient :: Int -> Int -> Int
divideSumTotient lower upper
    | (upper-lower) < 5 = sum (parMap rdeepseq euler [lower, lower+1 .. upper])
    | otherwise         = divideSumTotient' (length [lower, lower+1 .. upper])  [lower, lower+1 .. upper]

divideSumTotient' :: Int -> [Int] -> Int
divideSumTotient' _ []     = 0
divideSumTotient' size (x:xs)
    | (size) < 5        = sum (map euler (x:xs))
    | otherwise         = left `par` right `pseq` (left + right)
                where mid   = size `div` 2
                      left  = divideSumTotient' mid (take mid (x:xs))
                      right = divideSumTotient' (mid-1) (drop (mid+1) (x:xs))

-- divideSumTotient' lower upper [] = 0
-- divideSumTotient' lower upper (x:xs) =
--                 if (upper - lower) < 5
--                     then sum (map euler (x:xs))
--                 else
--                     left `par` right `pseq` (left + right)
--                         where size  = upper - lower
--                               mid = size `div` 2
--                               list  = (drop lower (take upper (x:xs)))
--                               left  = divideSumTotient' lower mid (take mid list)
--                               right = divideSumTotient' (mid+1) upper (drop (mid+1) list)

---------------------------------------------------------------------------
-- euler
---------------------------------------------------------------------------
-- The euler n function
-- 1. Generates a list [1,2,3, ... n-1,n]
-- 2. Select only those elements of the list that are relative prime to n
-- 3. Returns a count of the number of relatively prime elements

euler :: Int -> Int
-- euler n = length (filter (relprime n) [1 .. n-1])
euler n = length [x | x <- [1 .. n-1], relprime n x]

-- parFilter f x:xs = concat

-- relprimeToList n x xs
--  | relprime n x = x:xs
--  | otherwise    = xs

---------------------------------------------------------------------------
-- relprime
---------------------------------------------------------------------------
-- The relprime function returns true if it's arguments are relatively
-- prime, i.e. the highest common factor is 1.

relprime :: Int -> Int -> Bool
relprime x y = hcf x y == 1

---------------------------------------------------------------------------
-- hcf
---------------------------------------------------------------------------
-- The hcf function returns the highest common factor of 2 integers

hcf :: Int -> Int -> Int
hcf x 0 = x
hcf x y = hcf y (rem x y)

---------------------------------------------------------------------------
-- Interface Section
---------------------------------------------------------------------------

main = do args <- getArgs
          let
            lower = read (args!!0) :: Int -- lower limit of the interval
            upper = read (args!!1) :: Int -- upper limit of the interval
          hPutStrLn stderr ("Sum of Totients between [" ++
                  (show lower) ++ ".." ++ (show upper) ++ "] is " ++
                   show (sumTotient lower upper))
