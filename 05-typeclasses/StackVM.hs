module StackVM (StackVal(..), StackExp(..), Stack, Program, stackVM) where

-- Values that may appear in the stack. Such a value will also be
-- returned by the stackVM program execution function.
data StackVal = IVal Integer | BVal Bool | Void deriving Show

-- The various expressions our VM understands.
data StackExp = PushI Integer
              | PushB Bool
              | Add
              | Mul
              | And
              | Or
                deriving Show

type Stack   = [StackVal]
type Program = [StackExp]

-- Execute the given program. Returns either an error message or the
-- value on top of the stack after execution.
stackVM :: Program -> Either String StackVal
stackVM = execute []

errType :: String -> Either String a
errType op = Left $ "Encountered '" ++ op ++ "' opcode with ill-typed stack."

errUnderflow :: String -> Either String a
errUnderflow op = Left $ "Stack underflow with '" ++ op ++ "' opcode."

-- Execute a program against a given stack.
execute :: Stack -> Program -> Either String StackVal
execute [] []                               = Right Void
execute (s:_) []                            = Right s

execute s (PushI x : xs)                    = execute (IVal x : s) xs
execute s (PushB x : xs)                    = execute (BVal x : s) xs

