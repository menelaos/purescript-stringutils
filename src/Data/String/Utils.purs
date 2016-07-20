module Data.String.Utils
  ( NormalizationForm(..)
  , charAt
  , codePointAt
  , endsWith
  , endsWith'
  , escapeRegex
  , filter
  , includes
  , length
  , mapChars
  , normalize
  , normalize'
  , replaceAll
  , startsWith
  , startsWith'
  , stripChars
  , toCharArray
  )
where

import Data.Either       (fromRight)
import Data.Maybe        (Maybe(Just, Nothing))
import Data.String       (fromCharArray)
import Data.String.Regex (Regex, RegexFlags, noFlags, replace, regex)
import Partial.Unsafe    (unsafePartial)
import Prelude

import Data.Array as Array

-- | Return the character at the given index, if the index is within bounds.
-- | Note that this function handles Unicode as you would expect.
-- | If you want a simple wrapper around JavaScript's `String.prototype.charAt`
-- | method, you should use the `Data.String.charAt` function from
-- | `purescript-strings.`
-- |
-- | Example:
-- | ```purescript
-- | -- Data.String.Utils.charAt
-- | charAt 2 "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†" == Just '𝕣'
-- | -- Data.String.charAt
-- | charAt 2 "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†" == Just '�'
-- | ```
charAt :: Int -> String -> Maybe Char
charAt n str = Array.index (toCharArray str) n

-- | Return the Unicode code point value of the character at the given index,
-- | if the index is within bounds.
codePointAt :: Int -> String -> Maybe Int
codePointAt = _codePointAt Just Nothing

foreign import _codePointAt
  :: (∀ a. a -> Maybe a)
  -> (∀ a. Maybe a)
  -> Int
  -> String
  -> Maybe Int

-- | Determine whether the second string ends with the first one.
foreign import endsWith :: String -> String -> Boolean

-- | Determine whether the second string ends with the first one
-- | but search as if the string were only as long as the given argument.
endsWith' :: String -> Int -> String -> Boolean
endsWith' = endsWithP

foreign import endsWithP :: String -> Int -> String -> Boolean

-- | Escape a string so that it can be used as a literal string within a regular
-- | expression.
foreign import escapeRegex :: String -> String

-- | Keep only those characters that satisfy the predicate.
filter :: (Char -> Boolean) -> String -> String
filter p = fromCharArray <<< Array.filter p <<< toCharArray

-- | Determine whether the second arguments contains the first one.
foreign import includes :: String -> String -> Boolean

-- | Return the number of Unicode code points in a string.
-- | Note that this function correctly accounts for Unicode symbols that
-- | are made up of surrogate pairs. If you want a simple wrapper around
-- | JavaScript's `string.length` property, you should use the
-- | `Data.String.length` function from `purescript-strings`.
-- |
-- | ```purescript
-- | length "PureScript" == 10
-- | length "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†" == 10    -- 14 with `Data.String.length`
-- | ```
foreign import length :: String -> Int

-- | Return the string obtained by applying the mapping function to each
-- | character (i.e. Unicode code point) of the input string.
-- | Note that this is probably not what you want as Unicode code points are
-- | not necessarily the same as user-perceived characters (grapheme clusters).
-- | Only use this function if you know what you are doing.
-- |
-- | Example:
-- | ```purescript
-- | -- Mapping over what appears to be six characters...
-- | mapChars (const 'x') "Åström" === "xxxxxxxx" -- See? Don't use this!
-- | ```
mapChars :: (Char -> Char) -> String -> String
mapChars f = fromCharArray <<< map f <<< toCharArray

-- | Return the `Normalization Form C` of a given string.
-- | This is the form that is recommended by the W3C.
foreign import normalize :: String -> String

-- | Possible Unicode Normalization Forms
data NormalizationForm = NFC | NFD | NFKC | NFKD

instance showNormalizationForm :: Show NormalizationForm where
  show NFC  = "NFC"
  show NFD  = "NFD"
  show NFKC = "NFKC"
  show NFKD = "NFKD"

-- | Return a given Unicode Normalization Form of a string.
normalize' :: NormalizationForm -> String -> String
normalize' = _normalizeP <<< show

foreign import _normalizeP :: String -> String -> String

-- | Replace all occurences of the first argument with the second argument.
replaceAll :: String -> String -> String -> String
replaceAll old new str = replace (mkRegex old) new str
-- replaceAll old = replace (mkRegex old)
  where
    -- Helper function to construct a `Regex` from an input string
    mkRegex :: String -> Regex
    mkRegex str = unsafePartial (fromRight (regex (escapeRegex str) flags))

    -- Make sure that ALL occurrences and not only the first one get replaced
    flags :: RegexFlags
    flags = noFlags { global = true }

-- | Determine whether the second argument starts with the first one.
foreign import startsWith :: String -> String -> Boolean

-- | Determine whether a string starts with a certain substring at a given
-- | position.
startsWith' :: String -> Int -> String -> Boolean
startsWith' = startsWithP

foreign import startsWithP :: String -> Int -> String -> Boolean

-- | Strip a set of characters from a string.
-- | This function is case-sensitive.
-- |
-- | ```purescript
-- | stripChars "aeiou" "PureScript" == "PrScrpt"
-- | stripChars "AEIOU" "PureScript" == "PureScript"
-- | ```
foreign import stripChars :: String -> String -> String

-- | Convert a string to an array of Unicode code points.
-- | Note that this function is different from
-- | `Data.String.toCharArray` in `purescript-strings` which
-- | converts a string to an array of 16-bit code units.
-- | The difference becomes apparent when converting strings
-- | that contain characters which are internally represented
-- | as surrogate pairs.
-- |
-- | Example:
-- | ```purescript
-- | -- Data.String.Utils
-- | toCharArray "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†" == ["ℙ", "∪", "𝕣", "ⅇ", "Ⴝ", "𝚌", "𝕣", "ⅈ", "𝚙", "†"]
-- |
-- | -- Data.String
-- | toCharArray "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†" == ["ℙ", "∪", "�", "�", "ⅇ", "Ⴝ", "�", "�", "�", "�", "ⅈ", "�", "�", "†"]
-- | ```
foreign import toCharArray :: String -> Array Char
