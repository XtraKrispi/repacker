module Component.ViewInstructions where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Supabase (Client)
import Types (GameId, InstructionsKey, SessionInfo)
import Data.Maybe (Maybe)

type Input =
  { client :: Client
  , gameId :: GameId
  , instructionsKey :: InstructionsKey
  , session :: Maybe SessionInfo
  }

type State = Input

data Action = Initialize

component :: forall query output m. MonadEffect m => MonadAff m => H.Component query Input output m
component = H.mkComponent
  { initialState: identity
  , eval: H.mkEval H.defaultEval
  , render
  }

render :: forall action slots m. MonadAff m => MonadEffect m => State -> H.ComponentHTML action slots m
render _ = HH.div [ HP.class_ (H.ClassName "flex flex-col gap-4 py-16 items-center text-base-content/70") ]
  [ HH.h2 [ HP.class_ (H.ClassName "text-2xl font-bold text-primary") ]
      [ HH.text "View mode coming soon" ]
  , HH.p_ [ HH.text "A read-only walkthrough of these packing instructions will live here." ]
  ]
