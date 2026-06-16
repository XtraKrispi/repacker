module FFI.Supabase.Auth where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Nullable (Nullable)
import Data.Nullable as Nullable
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFn2, runEffectFn2)
import Foreign (Foreign)
import Supabase (AuthError, Client, Session, User, AuthResponse)
import Supabase.Auth (UserEmail(..))
import Yoga.JSON (write)

type InternalAuthResult =
  { user :: Nullable User
  , session :: Nullable Session
  }

type InternalAuthResponse =
  { data :: InternalAuthResult
  , error :: Nullable AuthError
  }

convertResponse :: InternalAuthResponse -> AuthResponse
convertResponse { data: d, error: err } =
  { data: { user: Nullable.toMaybe d.user, session: Nullable.toMaybe d.session }
  , error: Nullable.toMaybe err
  }

foreign import signInWithOtpImpl :: EffectFn2 Client Foreign (Promise InternalAuthResponse)

sendOtpToEmailWithRedirect :: { email :: UserEmail, redirectTo :: String } -> Client -> Aff AuthResponse
sendOtpToEmailWithRedirect { email: UserEmail email, redirectTo } client =
  runEffectFn2 signInWithOtpImpl client (write { email, options: { emailRedirectTo: redirectTo } }) # Promise.toAffE <#> convertResponse