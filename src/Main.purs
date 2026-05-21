module Main where

import Prelude

import Component.Router as Router
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.VDom.Driver (runUI)
import Route (Route(..), routeCodec)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  io <- runUI Router.component HomeR body
  void $ liftEffect $ matchesWith (parse routeCodec)
    ( \mOld mnew ->
        when (mOld /= Just mnew) $ do
          log $ "Changing routes from: " <> (show mOld) <> " to: " <> (show mnew)
          launchAff_ do
            _ <- io.query (Router.ChangeRoute mnew unit)
            pure unit
    )