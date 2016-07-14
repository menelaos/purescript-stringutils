module Data.Char.Utils
  ( toCodePoint )
where


-- | Returns the Unicode code point of a character.
-- |
-- | Example:
-- | ```purescript
-- | toCodePoint '∀' == 8704
-- | ```
foreign import toCodePoint :: Char -> Int
