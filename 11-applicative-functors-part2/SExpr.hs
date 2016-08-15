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

