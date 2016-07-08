module Test.Data.String.Utils
  ( testStringUtils )
where

import Control.Monad.Eff.Console (log)
import Data.Maybe                (Maybe (Just, Nothing))
import Data.String.Utils         ( codePointAt, escapeRegex, filter, replaceAll
                                 , startsWith
                                 )
import Prelude
import Test.StrongCheck          (Result, SC, (===), assert, quickCheck)

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

  log "escapeRegex"
  assert $ escapeRegex "."  === "\\."
  assert $ escapeRegex "*"  === "\\*"
  assert $ escapeRegex "+"  === "\\+"
  assert $ escapeRegex "?"  === "\\?"
  assert $ escapeRegex "^"  === "\\^"
  assert $ escapeRegex "$"  === "\\$"
  assert $ escapeRegex "{"  === "\\{"
  assert $ escapeRegex "}"  === "\\}"
  assert $ escapeRegex "("  === "\\("
  assert $ escapeRegex ")"  === "\\)"
  assert $ escapeRegex "|"  === "\\|"
  assert $ escapeRegex "["  === "\\["
  assert $ escapeRegex "]"  === "\\]"
  assert $ escapeRegex "\\" === "\\\\"

  log "filter"
  let
    filterIdProp :: String -> Result
    filterIdProp str = filter (const true) str === str

    filterNukeProp :: String -> Result
    filterNukeProp str = filter (const false) str === ""

    filterIdempotenceProp :: (Char -> Boolean) -> String -> Result
    filterIdempotenceProp f str = filter f (filter f str) === filter f str

    filterDistributiveProp :: (Char -> Boolean) -> String -> String -> Result
    filterDistributiveProp f a b =
      filter f (a <> b) === filter f a <> filter f b

    filterEmptyStringProp :: (Char -> Boolean) -> Result
    filterEmptyStringProp f = filter f "" === ""

  quickCheck filterIdProp
  quickCheck filterIdempotenceProp
  quickCheck filterDistributiveProp
  quickCheck filterEmptyStringProp
  quickCheck filterNukeProp

  log "replaceAll"
  let
    replaceAllIdProp :: String -> String -> Result
    replaceAllIdProp old str = replaceAll old old str === str

  assert $ replaceAll "." "" "Q.E.D." === "QED"
  quickCheck replaceAllIdProp

  log "startsWith"
  let
    startsWithSubsetProp :: String -> Result
    startsWithSubsetProp str = startsWith str str === true

    startsWithEmptyStringProp :: String -> Result
    startsWithEmptyStringProp str = startsWith "" str === true

  assert $ startsWith "Pure" "PureScript" === true
  quickCheck startsWithSubsetProp
  quickCheck startsWithEmptyStringProp
