module Main where

import Prelude

import Component.Router as Router
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FFI.Supabase.Client (createClientWithPasskey)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Route (Route(..), routeCodec)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Supabase (SupabaseAnonKey(..), SupabaseUrl(..))

main :: Effect Unit
main = do
  client <- createClientWithPasskey (SupabaseUrl "https://jruvwolatohqkqxcujjc.supabase.co") (SupabaseAnonKey "sb_publishable_8O5gqGJwgpMdY20XcYoz-Q_vMYzThpZ")
  runHalogenAff do
    body <- awaitBody
    io <- runUI Router.component { initialRoute: HomeR, client } body
    void $ liftEffect $ matchesWith (parse routeCodec)
      ( \mOld mnew ->
          when (mOld /= Just mnew) $ do
            log $ "Changing routes from: " <> (show mOld) <> " to: " <> (show mnew)
            launchAff_ do
              _ <- io.query (Router.ChangeRoute mnew unit)
              pure unit
      )