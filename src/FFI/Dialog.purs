module FFI.Dialog where

import Prelude

import Effect (Effect)

foreign import openModal :: String -> Effect Unit

foreign import close :: String -> Effect Unit