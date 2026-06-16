module FFI.Env where

import Types (Environment(..))

foreign import nodeEnv :: String

environment :: Environment
environment = case nodeEnv of
  "production" -> Production
  _ -> Development