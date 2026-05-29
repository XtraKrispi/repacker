module FFI.Supabase.Client where

import Supabase (Client, SupabaseAnonKey(..), SupabaseUrl(..))

import Data.Newtype (un)
import Effect (Effect)
import Effect.Aff.Compat (EffectFn2, runEffectFn2)

foreign import createClientWithPasskeyImpl :: EffectFn2 String String Client

createClientWithPasskey :: SupabaseUrl -> SupabaseAnonKey -> Effect Client
createClientWithPasskey url key = runEffectFn2 createClientWithPasskeyImpl (un SupabaseUrl url) (un SupabaseAnonKey key)