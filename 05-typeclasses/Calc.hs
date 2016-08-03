{-# OPTIONS_GHC -Wall -Werror #-}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM
import Data.Maybe
import qualified Data.Map as M

-------------------------------------------------------------------------------
-- Exercise 1

-- > eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20
eval :: ExprT -> Integer
eval (ExprT.Lit i) = i
eval (ExprT.Add a b) = eval a + eval b
eval (ExprT.Mul a b) = eval a * eval b

-------------------------------------------------------------------------------
-- Exercise 2

-- evaluates arithmetic expressions given as a String,
-- producing Nothing for inputs which are not well-formed expressions,
-- and Just n for well-formed inputs that evaluate to n
evalStr :: String -> Maybe Integer
evalStr = fmap eval . parseExp ExprT.Lit ExprT.Add ExprT.Mul

-------------------------------------------------------------------------------
-- Exercise 3

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

-------------------------------------------------------------------------------
-- Exercise 4

-- Should work like original calculator
instance Expr Integer where
  lit = id  -- if this is confusing look at the type signatur of the typeclass
  add = (+)
  mul = (*)

instance Expr Bool where
  lit x
    | x <= 0    = False
    | otherwise = True
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax x) (MinMax y)= MinMax (max x y)
  mul (MinMax x) (MinMax y)= MinMax (min x y)


newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit x = Mod7 (x `mod` 7)
  add (Mod7 x) (Mod7 y)= Mod7 ((x + y) `mod` 7)
  mul (Mod7 x) (Mod7 y)= Mod7 ((x * y) `mod` 7)

-- tests
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp

testBool :: Maybe Bool
testBool = testExp

testMM :: Maybe MinMax
testMM = testExp

testSat :: Maybe Mod7
testSat = testExp

