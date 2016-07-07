module Data.String.Utils
  ( codePointAt
  , filter
  )
where

import Data.Maybe        (Maybe(Just, Nothing))
import Data.String       (fromCharArray, toCharArray)
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

-- | Keep only those characters that satisfy the predicate.
filter :: (Char -> Boolean) -> String -> String
filter p = fromCharArray <<< Array.filter p <<< toCharArray
