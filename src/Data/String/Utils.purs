module Data.String.Utils
  ( codePointAt
  , endsWith
  , endsWith'
  , escapeRegex
  , filter
  , replaceAll
  , startsWith
  , startsWith'
  )
where

import Data.Either       (fromRight)
import Data.Maybe        (Maybe(Just, Nothing))
import Data.String       (fromCharArray, toCharArray)
import Data.String.Regex (Regex, RegexFlags, noFlags, replace, regex)
import Partial.Unsafe    (unsafePartial)
import Prelude

import Data.Array as Array

-- | Returns the Unicode code point value of the character at the given index,
-- | if the index is within bounds.
codePointAt :: Int -> String -> Maybe Int
codePointAt = _codePointAt Just Nothing

foreign import _codePointAt
  :: (∀ a. a -> Maybe a)
  -> (∀ a. Maybe a)
  -> Int
  -> String
  -> Maybe Int

-- | Determines whether the second string ends with the first one.
foreign import endsWith :: String -> String -> Boolean

-- | Determines whether the second string ends with the first one
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

-- | Replaces all occurences of the first argument with the second argument.
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

-- | Determines whether the second argument starts with the first one.
foreign import startsWith :: String -> String -> Boolean

-- | Determines whether a string starts with a certain substring at a given
-- | position.
startsWith' :: String -> Int -> String -> Boolean
startsWith' = startsWithP

foreign import startsWithP :: String -> Int -> String -> Boolean
