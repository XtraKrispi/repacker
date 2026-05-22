module Component.Profile where

import Prelude

import Data.Either (either)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Database.Profile (fetchProfile, saveProfile)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (get, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Supabase (Client)
import Supabase.Auth.Types (UserId)
import Types (Profile)
import Web.Event.Event (Event, preventDefault)
import Web.UIEvent.MouseEvent (MouseEvent)

type Input =
  { client :: Client
  , userId :: UserId
  , isReadOnly :: Boolean
  }

type State =
  { client :: Client
  , userId :: UserId
  , isReadOnly :: Boolean
  , profile :: RemoteData String (Maybe Profile)
  , firstName :: String
  , lastName :: String
  }

data Action
  = Initialize
  | UpdateFirstName String
  | UpdateLastName String
  | SaveProfile Event

component :: forall query output m. MonadAff m => MonadEffect m => H.Component query Input output m
component = H.mkComponent
  { initialState
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  , render
  }

initialState :: Input -> State
initialState { client, userId, isReadOnly } =
  { client
  , userId
  , isReadOnly
  , profile: NotAsked
  , firstName: ""
  , lastName: ""
  }

handleAction :: forall slots output m. MonadAff m => MonadEffect m => Action -> H.HalogenM State Action slots output m Unit
handleAction Initialize = do
  { userId, client } <- get
  modify_ _ { profile = Loading }
  results <- liftAff $ fetchProfile client userId
  modify_ _
    { profile = RemoteData.fromEither results
    , firstName = either (const "") (maybe "" _.firstName) results
    , lastName = either (const "") (maybe "" _.lastName) results
    }
handleAction (UpdateFirstName str) = modify_ _ { firstName = str }
handleAction (UpdateLastName str) = modify_ _ { lastName = str }
handleAction (SaveProfile event) = do
  liftEffect $ preventDefault event
  { userId, client, firstName, lastName } <- get
  results <- liftAff $ saveProfile client userId { firstName, lastName }
  pure unit

render :: forall slots m. State -> H.ComponentHTML Action slots m
render state = HH.div [ HP.class_ (H.ClassName "flex flex-col gap-4") ]
  [ HH.h2 [ HP.class_ (H.ClassName "text-2xl font-bold") ]
      [ HH.text "Profile"
      ]
  , HH.div []
      [ case state.profile of
          NotAsked -> HH.text ""
          Loading -> HH.text "Loading"
          Success p | not state.isReadOnly -> HH.form [ HE.onSubmit SaveProfile ]
            [ HH.fieldset [ HP.class_ (H.ClassName "fieldset") ]
                [ HH.legend [ HP.class_ (H.ClassName "fieldset-legend") ] [ HH.text "First Name" ]
                , HH.input
                    [ HP.type_ InputText
                    , HP.class_ (H.ClassName "input")
                    , HP.placeholder "First Name"
                    , HP.value state.firstName
                    , HE.onValueInput UpdateFirstName
                    ]
                ]
            , HH.fieldset [ HP.class_ (H.ClassName "fieldset") ]
                [ HH.legend [ HP.class_ (H.ClassName "fieldset-legend") ] [ HH.text "Last Name" ]
                , HH.input
                    [ HP.type_ InputText
                    , HP.class_ (H.ClassName "input")
                    , HP.placeholder "Last Name"
                    , HP.value state.lastName
                    , HE.onValueInput UpdateLastName
                    ]
                ]
            , HH.button [ HP.class_ (H.ClassName "btn btn-primary") ]
                [ HH.text "Save" ]
            ]
          Success p | otherwise -> HH.div [] []
          Failure err -> HH.text ""
      ]
  ]