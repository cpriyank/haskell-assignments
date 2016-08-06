{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

{-# LANGUAGE FlexibleInstances #-}

module Fibonacci where

--------------------------------------------------------------------------------
-- Exercise 1

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = fmap fib [0..]


--------------------------------------------------------------------------------
-- Exercise 2
-- Couldn't think of something with only n additions so I had a look at
-- http://stackoverflow.com/questions/1105765/generating-fibonacci-numbers-in-haskell
-- I especially like fibs4

-- my idea which does not work
{-fibs2 :: [Integer]-}
{-fibs2 = fiba [1, 0]-}

{-fiba :: [Integer] -> [Integer]-}
{-fiba (a:b:ys) = fiba(a+b:a:b:ys)-}
{-fiba _ = []-}

fibs3 :: [Integer]
fibs3 = 0:1:zipWith (+) fibs3 (tail fibs3)

fibo :: Integer -> Integer -> [Integer]
fibo a b = a : fibo b (a+b)

fibs4 :: [Integer]
fibs4 = fibo 0 1

