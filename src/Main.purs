module Main where

import Prelude

import Component.Router as Router
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FFI.Supabase.Client (createClientWithPasskey)
import Foreign (Foreign)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Route (Route(..), routeCodec)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Supabase (SupabaseAnonKey(..), SupabaseUrl(..), UserId, getUser)
import Supabase.Auth (UserEmail)
import Supabase.Auth.Types (Timestamp)
import Types (SessionInfo)

mkSessionInfo
  :: { data ::
         { user ::
             Maybe
               { app_metadata :: Foreign
               , aud :: String
               , created_at :: Timestamp
               , email :: UserEmail
               , id :: UserId
               , role :: String
               , updated_at :: Timestamp
               , user_metadata :: Foreign
               }
         }
     , error ∷ Maybe { message :: String, status :: Maybe Int }
     }
  -> Maybe SessionInfo
mkSessionInfo d = (\u -> { userId: u.id, email: u.email, name: Nothing }) <$> d.data.user

main :: Effect Unit
main = do
  client <- createClientWithPasskey (SupabaseUrl "https://jruvwolatohqkqxcujjc.supabase.co") (SupabaseAnonKey "sb_publishable_8O5gqGJwgpMdY20XcYoz-Q_vMYzThpZ")
  runHalogenAff do
    session <- mkSessionInfo <$> getUser client
    log (show session)
    body <- awaitBody
    io <- runUI Router.component { initialRoute: HomeR, client, session } body
    void $ liftEffect $ matchesWith (parse routeCodec)
      ( \mOld mnew ->
          when (mOld /= Just mnew) $ do
            log $ "Changing routes from: " <> (show mOld) <> " to: " <> (show mnew)
            launchAff_ do
              _ <- io.query (Router.ChangeRoute mnew unit)
              pure unit
      )