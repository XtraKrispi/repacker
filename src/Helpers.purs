module Helpers where

import Prelude

import Data.Array ((!!))
import Data.Array as Data.Array
import Data.Maybe (fromMaybe)
import Data.String.CodeUnits (fromCharArray)
import Data.Unfoldable (replicateA)
import Effect (Effect)
import Effect.Random (randomInt)

-- | Generates a random string of length N using a provided custom character set
randomString :: Int -> Array Char -> Effect String
randomString len allowedChars = do
  let maxIdx = Data.Array.length allowedChars - 1
  -- Pick a random character index 'len' times
  randomIndices <- replicateA len (randomInt 0 maxIdx)
  -- Map indices back to characters, defaulting to ' ' if out of bounds
  let chars = map (\idx -> fromMaybe ' ' (allowedChars !! idx)) randomIndices
  pure (fromCharArray chars)