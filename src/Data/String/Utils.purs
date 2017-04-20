module Data.String.Utils
  ( NormalizationForm(..)
  , charAt
  , codePointAt
  , codePointAt'
  , endsWith
  , endsWith'
  , escapeRegex
  , filter
  , fromCharArray
  , includes
  , includes'
  , length
  , lines
  , mapChars
  , normalize
  , normalize'
  , repeat
  , replaceAll
  , startsWith
  , startsWith'
  , stripChars
  , stripDiacritics
  , toCharArray
  , unsafeCodePointAt
  , unsafeCodePointAt'
  , unsafeRepeat
  , words
  )
where

import Data.Either             (fromRight)
import Data.Maybe              (Maybe(Just, Nothing))
import Data.String.Regex       (Regex, replace, regex)
import Data.String.Regex.Flags (global)
import Partial.Unsafe          (unsafePartial)
import Prelude

import Data.Array as Array

-- | Return the character at the given index, if the index is within bounds.
-- | Note that this function handles Unicode as you would expect.
-- | If you want a simple wrapper around JavaScript's `String.prototype.charAt`
-- | method, you should use the `Data.String.charAt` function from
-- | `purescript-strings.`
-- | This function returns a `String` instead of a `Char` because PureScript
-- | `Char`s must be UTF-16 code units and hence cannot represent all Unicode
-- | code points.
-- |
-- | Example:
-- | ```purescript
-- | -- Data.String.Utils.charAt
-- | charAt 2 "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†" == Just "𝕣"
-- | -- Data.String.charAt
-- | charAt 2 "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†" == Just '�'
-- | ```
charAt :: Int -> String -> Maybe String
charAt n str = Array.index (toCharArray str) n

-- | Return the Unicode code point value of the character at the given index,
-- | if the index is within bounds.
-- | Note that this function handles Unicode as you would expect.
-- | If you want a simple wrapper around JavaScript's
-- | `String.prototype.codePointAt` method, you should use `codePointAt'`.
-- |
-- | Example:
-- | ```purescript
-- | codePointAt   0 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == Just 120792
-- | codePointAt   1 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == Just 120793
-- | codePointAt   2 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == Just 120794
-- | codePointAt  19 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == Nothing
-- |
-- | codePointAt'  0 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == Just 120793
-- | codePointAt'  1 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == Just 57304   -- Surrogate code point
-- | codePointAt'  2 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == Just 120794
-- | codePointAt' 19 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == Just 57313   -- Surrogate code point
-- | ```
codePointAt :: Int -> String -> Maybe Int
codePointAt = _codePointAt Just Nothing

foreign import _codePointAt
  :: (∀ a. a -> Maybe a)
  -> (∀ a. Maybe a)
  -> Int
  -> String
  -> Maybe Int

-- | Return the Unicode code point value of the character at the given index,
-- | if the index is within bounds.
-- | This function is a simple wrapper around JavaScript's
-- | `String.prototype.codePointAt` method. This means that if the index does
-- | not point to the beginning of a valid surrogate pair, the code unit at
-- | the index (i.e. the Unicode code point of the surrogate pair half) is
-- | returned instead.
-- | If you want to treat a string as an array of Unicode Code Points, use
-- | `codePointAt` instead.
-- |
-- | Example:
-- | ```purescript
-- | codePointAt'  0 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == Just 120793
-- | codePointAt'  1 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == Just 57304   -- Surrogate code point
-- | codePointAt'  2 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == Just 120794
-- | codePointAt' 19 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == Just 57313   -- Surrogate code point
-- |
-- | codePointAt   0 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == Just 120792
-- | codePointAt   1 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == Just 120793
-- | codePointAt   2 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == Just 120794
-- | codePointAt  19 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == Nothing
-- | ```
codePointAt' :: Int -> String -> Maybe Int
codePointAt' = _codePointAtP Just Nothing

foreign import _codePointAtP
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
-- | This function uses `String` instead of `Char` because PureScript
-- | `Char`s must be UTF-16 code units and hence cannot represent all Unicode
-- | code points.
filter :: (String -> Boolean) -> String -> String
filter p = fromCharArray <<< Array.filter p <<< toCharArray

-- | Convert an array of characters into a `String`.
-- | This function uses `String` instead of `Char` because PureScript
-- | `Char`s must be UTF-16 code units and hence cannot represent all Unicode
-- | code points.
-- |
-- | Example:
-- | ```purescript
-- | fromCharArray ["ℙ", "∪", "𝕣", "ⅇ", "Ⴝ", "𝚌", "𝕣", "ⅈ", "𝚙", "†"]
-- |   == "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†"
-- | ```
foreign import fromCharArray :: Array String -> String

-- | Determine whether the second arguments contains the first one.
-- |
-- | Example:
-- | ```purescript
-- | includes "Merchant" "The Merchant of Venice" === true
-- | includes "Duncan"   "The Merchant of Venice" === false
-- | ```
foreign import includes :: String -> String -> Boolean

-- | Determine whether the second string argument contains the first one,
-- | beginning the search at the given position.
-- | Note that this function handles Unicode as you would expect.
-- | Negative `position` values result in a search from the beginning of the
-- | string.
-- |
-- | Example:
-- | ```purescript
-- | includes' "𝟙"  1 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == true
-- | includes' "𝟙"  2 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == false
-- | includes' "𝟡" 10 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == false
-- | -- This behaviour is different from `String.prototype.includes`:
-- | -- "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡".includes("𝟡", 10) == true
-- | ```
includes' :: String -> Int -> String -> Boolean
includes' = includesP

foreign import includesP :: String -> Int -> String -> Boolean

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

-- | Split a string into an array of strings which were delimited by newline
-- | characters.
-- |
-- | Example:
-- | ```purescript
-- | lines "Action\nis\neloquence." == ["Action", "is", "eloquence."]
-- | ```
foreign import lines :: String -> Array String

-- | Return the string obtained by applying the mapping function to each
-- | character (i.e. Unicode code point) of the input string.
-- | Note that this is probably not what you want as Unicode code points are
-- | not necessarily the same as user-perceived characters (grapheme clusters).
-- | Only use this function if you know what you are doing.
-- | This function uses `String`s instead of `Char`s because PureScript
-- | `Char`s must be UTF-16 code units and hence cannot represent all Unicode
-- | code points.
-- |
-- | Example:
-- | ```purescript
-- | -- Mapping over what appears to be six characters...
-- | mapChars (const "x") "Åström" == "xxxxxxxx" -- See? Don't use this!
-- | ```
mapChars :: (String -> String) -> String -> String
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

-- | Return a string that contains the specified number of copies of the input
-- | string concatenated together. Return `Nothing` if the repeat count is
-- | negative or if the resulting string would overflow the maximum string size.
-- |
-- | Example:
-- | ```purescript
-- | repeat 3 "𝟞" == Just "𝟞𝟞𝟞"
-- | repeat (-1) "PureScript" == Nothing
-- | repeat 2147483647 "PureScript" == Nothing
-- | ```
repeat :: Int -> String -> Maybe String
repeat = _repeat Just Nothing

foreign import _repeat
  :: (∀ a. a -> Maybe a)
  -> (∀ a. Maybe a)
  -> Int
  -> String
  -> Maybe String

-- | Replace all occurences of the first argument with the second argument.
replaceAll :: String -> String -> String -> String
replaceAll old new str = replace (mkRegex old) new str
  where
    -- Helper function to construct a `Regex` from an input string
    mkRegex :: String -> Regex
    mkRegex str = unsafePartial (fromRight (regex (escapeRegex str) global))

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
-- | Example:
-- | ```purescript
-- | stripChars "aeiou" "PureScript" == "PrScrpt"
-- | stripChars "AEIOU" "PureScript" == "PureScript"
-- | ```
foreign import stripChars :: String -> String -> String

-- | Strip diacritics from a string.
-- |
-- | Example:
-- | ```purescript
-- | stripDiacritics "Ångström"        == "Angstrom"
-- | stripDiacritics "Crème Brulée"    == "Creme Brulee"
-- | stripDiacritics "Götterdämmerung" == "Gotterdammerung"
-- | stripDiacritics "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†"      == "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†"
-- | stripDiacritics "Raison d'être"   == "Raison d'etre"
-- | stripDiacritics "Týr"             == "Tyr"
-- | stripDiacritics "Zürich"          == "Zurich"
-- | ```
foreign import stripDiacritics :: String -> String

-- | Convert a string to an array of Unicode code points.
-- | Note that this function is different from
-- | `Data.String.toCharArray` in `purescript-strings` which
-- | converts a string to an array of 16-bit code units.
-- | The difference becomes apparent when converting strings
-- | that contain characters which are internally represented
-- | as surrogate pairs.
-- | This function uses `String`s instead of `Char`s because PureScript
-- | `Char`s must be UTF-16 code units and hence cannot represent all Unicode
-- | code points.
-- |
-- | Example:
-- | ```purescript
-- | -- Data.String.Utils
-- | toCharArray "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†"
-- |   == ["ℙ", "∪", "𝕣", "ⅇ", "Ⴝ", "𝚌", "𝕣", "ⅈ", "𝚙", "†"]
-- |
-- | -- Data.String
-- | toCharArray "ℙ∪𝕣ⅇႽ𝚌𝕣ⅈ𝚙†" ==
-- |   ['ℙ', '∪', '�', '�', 'ⅇ', 'Ⴝ', '�', '�', '�', '�', 'ⅈ', '�', '�', '†']
-- | ```
foreign import toCharArray :: String -> Array String

-- | Return the Unicode code point value of the character at the given index,
-- | if the index is within bounds.
-- | Note that this function handles Unicode as you would expect.
-- | If you want a simple (unsafe) wrapper around JavaScript's
-- | `String.prototype.codePointAt` method, you should use `unsafeCodePointAt'`.
-- |
-- | **Unsafe:** Throws runtime exception if the index is not within bounds.
-- |
-- | Example:
-- | ```purescript
-- | unsafeCodePointAt   0 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == 120792
-- | unsafeCodePointAt   1 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == 120793
-- | unsafeCodePointAt   2 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == 120794
-- | unsafeCodePointAt  19 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" -- Error
-- |
-- | unsafeCodePointAt'  0 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == 120793
-- | unsafeCodePointAt'  1 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == 57304   -- Surrogate code point
-- | unsafeCodePointAt'  2 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == 120794
-- | unsafeCodePointAt' 19 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == 57313   -- Surrogate code point
-- | ```
foreign import unsafeCodePointAt :: Int -> String -> Int

-- | Return the Unicode code point value of the character at the given index,
-- | if the index is within bounds.
-- | This function is a simple (unsafe) wrapper around JavaScript's
-- | `String.prototype.codePointAt` method. This means that if the index does
-- | not point to the beginning of a valid surrogate pair, the code unit at
-- | the index (i.e. the Unicode code point of the surrogate pair half) is
-- | returned instead.
-- | If you want to treat a string as an array of Unicode Code Points, use
-- | `unsafeCodePointAt` instead.
-- |
-- | Example:
-- | ```purescript
-- | unsafeCodePointAt'  0 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == 120793
-- | unsafeCodePointAt'  1 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == 57304   -- Surrogate code point
-- | unsafeCodePointAt'  2 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == 120794
-- | unsafeCodePointAt' 19 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == 57313   -- Surrogate code point
-- |
-- | unsafeCodePointAt   0 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == 120792
-- | unsafeCodePointAt   1 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == 120793
-- | unsafeCodePointAt   2 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" == 120794
-- | unsafeCodePointAt  19 "𝟘𝟙𝟚𝟛𝟜𝟝𝟞𝟟𝟠𝟡" -- Error
-- | ```
unsafeCodePointAt' :: Int -> String -> Int
unsafeCodePointAt' = unsafeCodePointAtP

foreign import unsafeCodePointAtP :: Int -> String -> Int

-- | Return a string that contains the specified number of copies of the input
-- | string concatenated together.
-- |
-- | **Unsafe:** Throws runtime exception if the repeat count is negative or if
-- | the resulting string would overflow the maximum string size.
foreign import unsafeRepeat :: Int -> String -> String

-- | Split a string into an array of strings which were delimited by white space
-- | characters.
-- |
-- | Example:
-- | ```purescript
-- | words "Action is eloquence." == ["Action", "is", "eloquence."]
-- | ```
foreign import words :: String -> Array String
