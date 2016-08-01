module Test.Input
  ( NegativeInt(..)
  , NonNegativeInt(..)
  )
where

import Prelude
import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen       (chooseInt)


newtype NegativeInt = NegativeInt Int
newtype NonNegativeInt = NonNegativeInt Int

instance arbNegativeInt :: Arbitrary NegativeInt where
  arbitrary = NegativeInt <$> chooseInt (-2147483648.0) (-1.0)

instance arbNonNegativeInt :: Arbitrary NonNegativeInt where
  arbitrary = NonNegativeInt <$> chooseInt (0.0) (2147483647.0)
