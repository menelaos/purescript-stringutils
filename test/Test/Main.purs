module Test.Main where

import Prelude
import Test.Data.Char.Utils   (testCharUtils)
import Test.Data.String.Utils (testStringUtils)

main = do
  testCharUtils
  testStringUtils
