module Test.Data.Char.Utils
  ( testCharUtils )
where

import Control.Monad.Eff.Console  (log)
import Data.Char.Utils            (fromCodePoint, toCodePoint)
import Data.Int                   (toNumber)
import Data.Maybe                 (Maybe(Just, Nothing), isJust)
import Prelude
import Test.StrongCheck           (Result, SC, (===), assert, quickCheck)
import Test.StrongCheck.Arbitrary (class Arbitrary, arbitrary)
import Test.StrongCheck.Gen       (chooseInt)

testCharUtils :: SC () Unit
testCharUtils = do
  log "fromCodePoint"
  let
    fromCodePointRangeProp :: CodePoint -> Result
    fromCodePointRangeProp (CodePoint n) = isJust (fromCodePoint n) === true

  assert $ fromCodePoint   97     === Just 'a'
  assert $ fromCodePoint 8704     === Just '∀'
  assert $ fromCodePoint (-1)     === Nothing
  assert $ fromCodePoint 0x110000 === Nothing
  quickCheck fromCodePointRangeProp

  log "toCodePoint"
  assert $ toCodePoint 'a' === 97
  assert $ toCodePoint '∀' === 8704

  log "fromCodePoint <<< toCodePoint == Just"
  let
    codePointIdentityProp :: Char -> Result
    codePointIdentityProp = (===) <$> fromCodePoint <<< toCodePoint <*> Just

  quickCheck codePointIdentityProp

-- We use a newtype in order to generate arbitrary code points with StrongCheck
newtype CodePoint = CodePoint Int

-- Unicode code points are in the range 0 .. U+10FFFF
instance arbCodePoint :: Arbitrary CodePoint where
  arbitrary = CodePoint <$> chooseInt 0.0 (toNumber 0x10FFFF)
