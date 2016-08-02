module Test.Input
  ( NegativeInt(..)
  , NonNegativeInt(..)
  , WhiteSpaceChar(..)
  )
where

import Data.List                  (List, fromFoldable)
import Prelude
import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen       (chooseInt, elements)


newtype NegativeInt = NegativeInt Int
newtype NonNegativeInt = NonNegativeInt Int
newtype WhiteSpaceChar = WhiteSpaceChar Char

instance arbNegativeInt :: Arbitrary NegativeInt where
  arbitrary = NegativeInt <$> chooseInt (-2147483648.0) (-1.0)

instance arbNonNegativeInt :: Arbitrary NonNegativeInt where
  arbitrary = NonNegativeInt <$> chooseInt (0.0) (2147483647.0)

instance arbWhiteSpaceChar :: Arbitrary WhiteSpaceChar where
  arbitrary = WhiteSpaceChar <$> elements ' ' whiteSpaceChars

whiteSpaceChars :: List Char
whiteSpaceChars = fromFoldable
  [ ' ', '\f', '\n', '\r', '\t', '\v', '\x00A0', '\x1680', '\x180E', '\x2000'
  , '\x2001', '\x2002', '\x2003', '\x2004', '\x2005', '\x2006', '\x2007'
  , '\x2008', '\x2009', '\x200A', '\x2028', '\x2029', '\x202F', '\x205F'
  , '\x3000', '\xFEFF'
  ]
