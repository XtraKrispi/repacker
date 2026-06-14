module Component.MySubmissions where

import Prelude

import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Supabase (Client)
import Types (SessionInfo)

-- TODO: Plumb this through

type CoreData = (client :: Client, session :: SessionInfo)

type Input = { | CoreData }

type State = { | CoreData }

component :: forall query output m. MonadEffect m => MonadAff m => H.Component query Input output m
component = H.mkComponent
  { initialState
  , eval: H.mkEval H.defaultEval
  , render
  }

initialState :: Input -> State
initialState { client, session } = { client, session }

render :: forall action slots m. MonadEffect m => MonadAff m => State -> H.ComponentHTML action slots m
render state = HH.div [] [ HH.text "My Submissions" ]