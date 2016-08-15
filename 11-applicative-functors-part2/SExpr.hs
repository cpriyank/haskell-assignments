{- CIS 194 HW 11
   due Monday, 8 April
-}

module SExpr where

import AParser

import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
------------------------------------------------------------

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

oneOrMore :: Parser a -> Parser [a]
oneOrMore p = (:) <$> p <*> zeroOrMore p
-- equivalent to:
{-oneOrMore p = liftA2 (:) p (zeroOrMore p-}

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = liftA2 (:) (satisfy isAlpha) (zeroOrMore $ satisfy isAlphaNum)

