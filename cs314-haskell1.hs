-- source /common/system/haskell/.ghcup/env

-- For this assignment, replace the incomplete definitions
-- with working definitions.
-- 
-- Type signatures have been provided to help improve
-- error messages and ensure some consistency in answers.
-- Do not change or remove the type signatures.
--
-- Examples are given for the functions as a guide, but
-- are obviously not exhaustive. Your functions should be
-- correct for all conceiveable inputs.

module HW1 where

-- Example. Write a function mean3 that returns the mean
-- of three Doubles.
--
-- ghci> mean3 0 10 14
-- 8.0

mean3 :: Double -> Double -> Double -> Double
mean3 = undefined

-- to answer, replace the line above with:
-- mean3 a b c = (a + b + c) / 3









-- 1. Write a function median3 that returns the median
-- of three Integers.
--
-- ghci> median3 1 3 2
-- 2
-- ghci> median3 5 5 100
-- 5

median3 :: Integer -> Integer -> Integer -> Integer
median3 a b c = (a + b + c) -  maximum[a, b, c] - minimum[a, b, c]
              --add all 3, remove max and min, left with median

-- 2. Write a function spread that applies a value to
-- each of a list of functions, returning a list of results.
--
-- ghci> spread 2 [(+1), (/2), (*3), negate]
-- [3,1,6,-2]
-- ghci> spread [1,2,3] [drop 1, take 2, reverse]
-- [[2,3],[1,2],[3,2,1]]
-- ghci> spread [1,4,3,2] [sum, product, minimum, length]
-- [10,24,1,4]
-- ghci> spread 6 []
-- []

spread :: a -> [a -> b] -> [b]
spread _ []      = []
       --if given an empty list, return an empty list no matter the value v

spread v (h : t) = h v : spread v t
       --on the value v, call "h v" first (head of list, first function on v)
       --then, recursively call spread again on spread with the tail of the list
       --(remaining function calls in list) with the same value v

-- 3. Write a function chain that applies a list of
-- functions to a value, such that 
--    chain [f,g,h] x = f (g (h x))
--
-- ghci> chain [] 10
-- 10
-- ghci> chain [(+1),(*2)] 10
-- 21
-- ghci> chain [(*2),(+1)] 10
-- 22
-- ghci> chain [reverse, drop 2, reverse] [1,2,3,4,5]
-- [1,2,3]

chain :: [a -> a] -> a -> a
chain [] x      = x
chain (h : t) x = h (chain t x)
      --given a list, recursively call chain on the tail until the last element of it

-- 4. Write a function hailstone that returns the hailstone
-- sequence for a positive number n.
--
-- The hailstone sequence is created iteratively. For a
-- given number n, the next element will be n `div` 2 if
-- n is even and 3*n+1 if n is odd. The sequence ends at 1.
--
-- For zero and negative inputs, return an empty list.
--
-- ghci> hailstone 2
-- [2,1]
-- ghci> hailstone 3
-- [3,10,5,16,8,4,2,1]
--
-- Note that a hailstone sequence is believed to always
-- reach 1 eventually, this is a famously unproven conjecture.
--
-- Suggestion: The prelude includes functions even and odd.
-- Reminder: Use div for integer division.

hailstone :: Integer -> [Integer]
hailstone n
   | n < 1  = []
   | n == 1 = n : []
   | even n = n : hailstone (n `div` 2)
   | odd n  = n : hailstone (3 * n + 1)

-- 5. Write a function that takes two lists of integers and 
-- returns a list of the pair-wise minimal values. That is,
-- the nth element is the smaller of the nth elements in the
-- input lists.
--
-- If one list is longer than the other, you may discard
-- the excess values.
--
-- ghci> mins [1,3,2,5] [2,1,7,6]
-- [1,1,2,5]
-- ghci> mins [8,9,-2,8,6] [1,1,1,1,1]
-- [1,1,-2,1,1]
-- ghci> mins [85,9,17] [2,19,25,6]
-- [2,9,17]
--
-- Suggestion: the prelude includes a function min.

mins :: [Integer] -> [Integer] -> [Integer]
mins [] [] = []
mins [] is = []
mins js [] = []
mins is js = min (head is) (head js) : mins (tail is) (tail js)
     --if one list is longer than the other, return an empty list each time
     --otherwise, find min between the head of each and recursively call mins on the
     --remaining tail
