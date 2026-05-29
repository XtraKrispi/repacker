module Component.Profile where

import Prelude

import Component.Helpers (addToast)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Database.Profile (fetchProfile, saveProfile)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (get, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Store as S
import Supabase (Client)
import Supabase.Auth.Types (UserId)
import Types (Profile, SessionInfo)
import Web.Event.Event (Event, preventDefault)

type CoreData =
  ( client :: Client
  , session :: Maybe SessionInfo
  , userId :: UserId
  )

type Input =
  { | CoreData
  }

type State =
  { profile :: RemoteData String (Maybe Profile)
  , firstName :: String
  , lastName :: String
  , username :: String
  , isReadOnly :: Boolean
  | CoreData
  }

data Action
  = Initialize
  | UpdateFirstName String
  | UpdateLastName String
  | UpdateUsername String
  | SaveProfile Event

component :: forall query output m. MonadAff m => MonadEffect m => MonadStore S.Action S.Store m => H.Component query Input output m
component = H.mkComponent
  { initialState
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  , render
  }

initialState :: Input -> State
initialState { client, userId, session } =
  { client
  , userId
  , session
  , profile: NotAsked
  , firstName: ""
  , lastName: ""
  , username: ""
  , isReadOnly: Just userId /= (_.userId <$> session)
  }

handleAction :: forall slots output m. MonadAff m => MonadEffect m => MonadStore S.Action S.Store m => Action -> H.HalogenM State Action slots output m Unit
handleAction Initialize = do
  { userId, client } <- get
  modify_ _ { profile = Loading }
  results <- liftAff $ fetchProfile client userId
  modify_ _
    { profile = RemoteData.fromEither results
    , firstName = either (const "") (maybe "" _.firstName) results
    , lastName = either (const "") (maybe "" _.lastName) results
    , username = either (const "") (maybe "" _.username) results
    }
handleAction (UpdateFirstName str) = modify_ _ { firstName = str }
handleAction (UpdateLastName str) = modify_ _ { lastName = str }
handleAction (UpdateUsername str) = modify_ _ { username = str }
handleAction (SaveProfile event) = do
  liftEffect $ preventDefault event
  { userId, client, firstName, lastName, username, session } <- get
  case session of
    Just s -> do
      results <- liftAff $ saveProfile client userId { firstName, lastName, username, email: s.email }
      case results of
        Right _ -> addToast { message: "Your profile has been updated.", severity: S.Success }
        Left _err -> addToast { message: "There was an issue saving your profile, please try again.", severity: S.Error }
    Nothing -> pure unit

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
                [ HH.legend [ HP.class_ (H.ClassName "fieldset-legend") ] [ HH.text "Email" ]
                , HH.input
                    [ HP.type_ InputText
                    , HP.readOnly true
                    , HP.class_ (H.ClassName "input")
                    , HP.value $ maybe "" (unwrap <<< _.email) p
                    ]
                ]
            , HH.fieldset [ HP.class_ (H.ClassName "fieldset") ]
                [ HH.legend [ HP.class_ (H.ClassName "fieldset-legend") ] [ HH.text "Username" ]
                , HH.input
                    [ HP.type_ InputText
                    , HP.class_ (H.ClassName "input")
                    , HP.placeholder "Username"
                    , HP.value state.username
                    , HE.onValueInput UpdateUsername
                    ]
                ]
            , HH.fieldset [ HP.class_ (H.ClassName "fieldset") ]
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
          Success _p | otherwise -> HH.div [] [ HH.text "UPDATE ME FOR READ ONLY" ]
          Failure _err -> HH.text "UPDATE ME FOR FAILURE"
      ]
  ]