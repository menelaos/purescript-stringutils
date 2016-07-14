module Test.Data.Char.Utils
  ( testCharUtils )
where

import Control.Monad.Eff.Console (log)
import Data.Char.Utils           (toCodePoint)
import Prelude
import Test.StrongCheck          (SC, (===), assert)

testCharUtils :: SC () Unit
testCharUtils = do
  log "toCodePoint"
  assert $ toCodePoint 'a' === 97
  assert $ toCodePoint '∀' === 8704
