module Data.Char.Utils
  ( fromCodePoint
  , toCodePoint
  )
where

import Data.Maybe (Maybe(Just, Nothing))


-- | Returns the character corresponding to the given Unicode code point and
-- | `Nothing` if the given number is outside the range 0 .. 0x10FFFF.
fromCodePoint :: Int -> Maybe Char
fromCodePoint = _fromCodePoint Just Nothing

foreign import _fromCodePoint
  :: (∀ a. a -> Maybe a)
  -> (∀ a. Maybe a)
  -> Int
  -> Maybe Char

-- | Returns the Unicode code point of a character.
-- |
-- | Example:
-- | ```purescript
-- | toCodePoint '∀' == 8704
-- | ```
foreign import toCodePoint :: Char -> Int
