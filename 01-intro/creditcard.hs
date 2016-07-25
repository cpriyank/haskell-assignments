{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE ViewPatterns #-}

module CreditCard where

-- Converts positive Integers to a list of their digits.
-- toDigits 1234 == [1,2,3,4]
-- toDigits 0 == []
-- toDigits (-17) == []
toDigits :: Integer -> [Integer]
toDigits n
  {-| n > 0   = reverse $ toDigitsRev n-}
  | n > 0     = toDigits (n `div` 10) ++ [n `mod` 10]
  | otherwise = []

-- Converts positive Integers to a reversed list of their digits.
-- toDigits 1234 == [4,3,2,1]
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n > 0     = n `mod` 10 : toDigitsRev (n `div` 10)
  | otherwise = []

-- Return digit of Integer n at specified place exponent of 10
-- digitAtPlace 1234 4 = 1
-- digitAtPlace 1234 4 = 1
digitAtPlace :: Integer -> Integer -> Integer
digitAtPlace n expo
  | expo >= 0 = (n `div` (10^expo)) `mod` 10
  | otherwise = -1

