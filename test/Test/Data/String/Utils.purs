module Test.Data.String.Utils
  ( testStringUtils )
where

import Control.Monad.Eff.Console (log)
import Data.Maybe                (Maybe (Just, Nothing))
import Data.String               as Data.String
import Data.String.Utils         ( codePointAt, endsWith, endsWith'
                                 , escapeRegex, filter, length, replaceAll
                                 , startsWith, startsWith', stripChars
                                 , toCharArray
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

  log "endsWith"
  let
    endsWithSubsetProp :: String -> Result
    endsWithSubsetProp str = endsWith str str === true

    endsWithEmptyStringProp :: String -> Result
    endsWithEmptyStringProp str = endsWith "" str === true

  assert $ endsWith "Script" "PureScript" === true
  assert $ endsWith "happy ending" "火垂るの墓" === false
  quickCheck endsWithSubsetProp
  quickCheck endsWithEmptyStringProp

  log "endsWith'"
  let
    -- Note that the `position` argument is translated to
    -- `min(max(pos, 0), len)``
    -- Cf. http://www.ecma-international.org/ecma-262/6.0/#sec-string.prototype.endswith
    endsWith'EmptyStringProp :: String -> Int -> Result
    endsWith'EmptyStringProp str n = endsWith' "" n str === true

  assert $ endsWith' "Pure" 4 "PureScript" === true
  assert $ endsWith' "Script" 4 "PureScript" === false
  quickCheck endsWith'EmptyStringProp

  log "endsWith & endsWith'"
  let
    endsWith'LengthProp :: String -> String -> Result
    endsWith'LengthProp search str =
      endsWith' search (Data.String.length str) str === endsWith search str

  quickCheck endsWith'LengthProp

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

    allButPureScript :: Char -> Boolean
    allButPureScript 'ℙ' = true
    allButPureScript '∪' = true
    allButPureScript '𝕣' = true
    allButPureScript 'ⅇ' = true
    allButPureScript 'Ⴝ' = true
    allButPureScript '𝚌' = true
    allButPureScript '𝕣' = true
    allButPureScript 'ⅈ' = true
    allButPureScript '𝚙' = true
    allButPureScript '†' = true
    allButPureScript _ = false

  -- This assertion is to make sure that `filter` operates on code points
  -- and not code units.
  assert $ filter allButPureScript "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙† rocks!" === "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†"
  quickCheck filterIdProp
  quickCheck filterIdempotenceProp
  quickCheck filterDistributiveProp
  quickCheck filterEmptyStringProp
  quickCheck filterNukeProp

  log "length"
  let
    lengthNonNegativeProp :: String -> Result
    lengthNonNegativeProp str = length str >= 0 === true

  assert $ length "" === 0
  assert $ length "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†" === 10
  quickCheck lengthNonNegativeProp

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

  log "startsWith'"
  let
    -- Note that negative `position` arguments have the same effect as `0`
    -- Cf. http://www.ecma-international.org/ecma-262/6.0/#sec-string.prototype.startswith
    startsWith'EmptyStringProp :: String -> Int -> Result
    startsWith'EmptyStringProp str n = startsWith' "" n str === true

  assert $ startsWith' "Script" 4 "PureScript" === true
  quickCheck startsWith'EmptyStringProp

  log "startsWith & startsWith'"
  let
    startsWith'ZeroProp :: String -> String -> Result
    startsWith'ZeroProp searchString str =
      startsWith' searchString 0 str === startsWith searchString str

  quickCheck startsWith'ZeroProp

  log "stripChars"
  let
    stripCharsIdempotenceProp :: String -> String -> Result
    stripCharsIdempotenceProp chars =
      (===) <$> (stripChars chars <<< stripChars chars) <*> stripChars chars

    stripCharsEmptyStringProp :: String -> Result
    stripCharsEmptyStringProp = (===) <$> id <*> stripChars ""

  assert $ stripChars "Script" "JavaScript" === "Java" -- Seriously?
  assert $ stripChars "PURESCRIPT" "purescript" === "purescript"
  quickCheck stripCharsIdempotenceProp
  quickCheck stripCharsEmptyStringProp

  log "toCharArray"
  assert $ toCharArray "" === []
  assert $ toCharArray "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†" === ['ℙ', '∪', '𝕣', 'ⅇ', 'Ⴝ', '𝚌', '𝕣', 'ⅈ', '𝚙', '†']

