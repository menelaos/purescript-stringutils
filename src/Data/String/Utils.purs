module Data.String.Utils
  ( codePointAt
  )
where

import Data.Maybe        (Maybe(Just, Nothing))
import Prelude

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
