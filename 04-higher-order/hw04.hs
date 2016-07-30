{-# OPTIONS_GHC -Wall #-}

module Homework where

import Data.List
-- Exercise 2

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- should produce a balanced Tree using @foldr@
foldTree :: Eq a => [a] -> Tree a
foldTree xs = foldr (balancedInsert start) Leaf xs
  where start = floor (logBase 2 $ fromIntegral(length xs)::Double)

balancedInsert :: Int -> a -> Tree a -> Tree a
balancedInsert _ _ _ = Leaf
{-balancedInsert _ x (Node n left y right)-}
          {-| right == Leaf = Node n left y (Node (n-1) Leaf x Leaf)-}
          {-| otherwise = Node n (Node (n-1) Leaf x Leaf) y right-}
{-balancedInsert start x _ = Node  Leaf x Leaf-}

-------------------------------------------------------------------------------
-- Exercise 1

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum
      . filter even
      . takeWhile (/= 1)
      . iterate (\n -> if even n then n `div` 2 else 3*n+1)

