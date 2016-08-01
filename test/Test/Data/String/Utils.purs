module Test.Data.String.Utils
  ( testStringUtils )
where

import Control.Monad.Eff.Console (log)
import Data.Maybe                (Maybe (Just, Nothing))
import Data.String               as Data.String
import Data.String.Utils         ( NormalizationForm(NFC), charAt, codePointAt
                                 , codePointAt', endsWith, endsWith'
                                 , escapeRegex, filter, includes, length
                                 , mapChars, normalize, normalize', repeat
                                 , replaceAll, startsWith, startsWith'
                                 , stripChars, toCharArray
                                 )
import Prelude
import Test.Input                ( NegativeInt(NegativeInt)
                                 , NonNegativeInt(NonNegativeInt)
                                 )
import Test.StrongCheck          (Result, SC, (===), assert, quickCheck)

testStringUtils :: SC () Unit
testStringUtils = do
  log "charAt"
  let
    charAtEmptyStringProp :: Int -> Result
    charAtEmptyStringProp n = charAt n "" === Nothing

  assert $ charAt 2 "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†" === Just '𝕣'
  quickCheck charAtEmptyStringProp

  log "codePointAt"
  assert $ codePointAt  0 ""           === Nothing
  assert $ codePointAt  0 "a"          === Just 97
  assert $ codePointAt  1 "a"          === Nothing
  assert $ codePointAt  0 "ab"         === Just 97
  assert $ codePointAt  1 "ab"         === Just 98
  assert $ codePointAt  2 "ab"         === Nothing
  assert $ codePointAt  0 "∀"          === Just 8704
  assert $ codePointAt  1 "∀ε"         === Just 949
  assert $ codePointAt  0 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" === Just 120792
  assert $ codePointAt  1 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" === Just 120793
  assert $ codePointAt 19 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" === Nothing

  log "codePointAt'"
  assert $ codePointAt'  0 ""           === Nothing
  assert $ codePointAt'  0 "a"          === Just 97
  assert $ codePointAt'  1 "a"          === Nothing
  assert $ codePointAt'  0 "ab"         === Just 97
  assert $ codePointAt'  1 "ab"         === Just 98
  assert $ codePointAt'  2 "ab"         === Nothing
  assert $ codePointAt'  0 "∀"          === Just 8704
  assert $ codePointAt'  1 "∀ε"         === Just 949
  assert $ codePointAt'  0 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" === Just 120792
  assert $ codePointAt'  1 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" === Just 57304
  assert $ codePointAt' 19 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" === Just 57313

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
  assert $ escapeRegex "."   === "\\."
  assert $ escapeRegex "*"   === "\\*"
  assert $ escapeRegex "+"   === "\\+"
  assert $ escapeRegex "?"   === "\\?"
  assert $ escapeRegex "^"   === "\\^"
  assert $ escapeRegex "$"   === "\\$"
  assert $ escapeRegex "{"   === "\\{"
  assert $ escapeRegex "}"   === "\\}"
  assert $ escapeRegex "("   === "\\("
  assert $ escapeRegex ")"   === "\\)"
  assert $ escapeRegex "|"   === "\\|"
  assert $ escapeRegex "["   === "\\["
  assert $ escapeRegex "]"   === "\\]"
  assert $ escapeRegex "-"   === "\\-"
  assert $ escapeRegex "\\"  === "\\\\"
  assert $ escapeRegex "A-Z" === "A\\-Z"

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

  log "includes"
  let
    includesSubsetProp :: String -> Result
    includesSubsetProp str = includes str str === true

    includesEmptyStringProp :: String -> Result
    includesEmptyStringProp str = includes "" str === true

  assert $ includes "Merchant" "The Merchant of Venice" === true
  assert $ includes "Duncan" "The Merchant of Venice" === false
  quickCheck includesSubsetProp
  quickCheck includesEmptyStringProp

  log "length"
  let
    lengthNonNegativeProp :: String -> Result
    lengthNonNegativeProp str = length str >= 0 === true

  assert $ length "" === 0
  assert $ length "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†" === 10
  quickCheck lengthNonNegativeProp

  log "mapChars"
  -- Mapping over individual characters (Unicode code points) in e.g.
  -- unnormalized strings results in unexpected (yet correct) behaviour.
  assert $ mapChars (const 'x') "Åström" === "xxxxxxxx"

  log "normalize"
  -- Due to incomplete fonts, the strings in the following assertions may
  -- appear to be different from one another.
  -- They are canonically equivalent, however.

  -- Å: U+00C5
  -- Å: U+212B
  assert $ normalize "Å" === normalize "Å"

  -- Åström: U+00C5        U+0073 U+0074 U+0072 U+00F6        U+006D
  -- Åström: U+0041 U+030A U+0073 U+0074 U+0072 U+006F U+0308 U+006D
  assert $ normalize "Åström" === normalize "Åström"

  -- á: U+00E1
  -- á: U+0061 U+0301
  assert $ normalize "á" === normalize "á"

  -- Amélie: U+0041 U+006d U+00e9        U+006c U+0069 U+0065
  -- Amélie: U+0041 U+006d U+0065 U+0301 U+006c U+0069 U+0065
  assert $ normalize "Amélie" === normalize "Amélie"

  -- ḱṷṓn: U+1E31 U+1E77 U+1E53                             U+006E
  -- ḱṷṓn: U+006B U+0301 U+0075 U+032D U+006F U+0304 U+0301 U+006E
  assert $ normalize "ḱṷṓn" === normalize "ḱṷṓn"

  log "normalize & normalize'"
  let
    nfcProp :: String -> Result
    nfcProp = (===) <$> normalize <*> normalize' NFC

  quickCheck nfcProp

  log "repeat"
  let
    repeatZeroProp :: String -> Result
    repeatZeroProp str = repeat 0 str === Just ""

    repeatOnceProp :: String -> Result
    repeatOnceProp = (===) <$> repeat 1 <*> Just

    repeatNegativeProp :: NegativeInt -> String -> Result
    repeatNegativeProp (NegativeInt n) str = repeat n str === Nothing

    repeatEmptyStringProp :: NonNegativeInt -> Result
    repeatEmptyStringProp (NonNegativeInt n) = repeat n "" === Just ""

  assert $ repeat 3 "𝟞" === Just "𝟞𝟞𝟞"
  assert $ repeat 2147483647 "𝟞" === Nothing
  quickCheck repeatZeroProp
  quickCheck repeatOnceProp
  quickCheck repeatNegativeProp
  quickCheck repeatEmptyStringProp

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

  assert $ stripChars "Script"     "JavaScript" === "Java" -- Seriously?
  assert $ stripChars "PURESCRIPT" "purescript" === "purescript"
  assert $ stripChars "a-z"        "-abc--xyz-" === "bcxy"
  quickCheck stripCharsIdempotenceProp
  quickCheck stripCharsEmptyStringProp

  log "toCharArray"
  assert $ toCharArray "" === []
  assert $ toCharArray "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†" === ['ℙ', '∪', '𝕣', 'ⅇ', 'Ⴝ', '𝚌', '𝕣', 'ⅈ', '𝚙', '†']

