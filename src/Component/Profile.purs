module Component.Profile where

import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Supabase (Client)
import Supabase.Auth (UserEmail)

type Input =
  { client :: Client
  , userEmail :: UserEmail
  , isReadOnly :: Boolean
  }

type State = { client :: Client, userEmail :: UserEmail, isReadOnly :: Boolean }

component :: forall query output m. MonadAff m => MonadEffect m => H.Component query Input output m
component = H.mkComponent { initialState, eval: H.mkEval H.defaultEval, render }

initialState :: Input -> State
initialState { client, userEmail, isReadOnly } = { client, userEmail, isReadOnly }

render :: forall action slots m. State -> H.ComponentHTML action slots m
render state = HH.div [] [ HH.text "Profile" ]