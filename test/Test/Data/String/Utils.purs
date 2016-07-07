module Test.Data.String.Utils
  ( testStringUtils )
where

import Control.Monad.Eff.Console (log)
import Data.Maybe                (Maybe (Just, Nothing))
import Data.String.Utils         (codePointAt)
import Prelude
import Test.StrongCheck          (SC, (===), assert)

testStringUtils :: SC () Unit
testStringUtils = do
  log "codePointAt"
  assert $ codePointAt 0 ""   === Nothing
  assert $ codePointAt 0 "a"  === Just 97
  assert $ codePointAt 1 "a"  === Nothing
  assert $ codePointAt 0 "ab" === Just 97
  assert $ codePointAt 1 "ab" === Just 98
  assert $ codePointAt 2 "ab" === Nothing
  assert $ codePointAt 0 "∀"  === Just 8704
  assert $ codePointAt 1 "∀ε" === Just 949
