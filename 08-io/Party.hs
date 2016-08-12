{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where

import Data.Monoid
import Data.Tree

import Employee

main :: IO ()
main = readFile "company.txt" >>= putStrLn . computeOutput

computeOutput :: String -> String
computeOutput = formatGL . maxFun . read

formatGL :: GuestList -> String
formatGL (GL lst fun) = "Total fun: " ++ show fun ++ "\n" ++ unlines employees
  where employees = map (\(Emp {empName = name}) -> name) lst

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL al af) (GL bl bf) = GL (al++bl) (af+bf)

-- | Adds Employee to the GuestList and update cached Fun score.
glCons :: Employee -> GuestList -> GuestList
glCons emp@(Emp {empFun = ef}) (GL lst gf) = GL (emp:lst) (ef+gf)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a b = if a > b then a else b

