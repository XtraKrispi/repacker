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
render state = HH.div [ HP.class_ (H.ClassName "max-w-md mx-auto w-full") ]
  [ HH.div [ HP.class_ (H.ClassName "card bg-base-200 shadow-xl") ]
      [ HH.div [ HP.class_ (H.ClassName "card-body") ]
          [ HH.h2 [ HP.class_ (H.ClassName "card-title text-2xl text-primary") ]
              [ HH.text "Profile" ]
          , case state.profile of
              NotAsked -> HH.text ""
              Loading -> HH.div [ HP.class_ (H.ClassName "flex justify-center items-center flex-col gap-2 py-8") ]
                [ HH.span [ HP.class_ (H.ClassName "loading loading-spinner loading-lg text-primary") ] []
                , HH.p [ HP.class_ (H.ClassName "text-base-content/70") ] [ HH.text "Loading profile..." ]
                ]
              Failure err -> HH.div [ HP.class_ (H.ClassName "alert alert-error") ]
                [ HH.span_ [ HH.text err ] ]
              Success p | not state.isReadOnly -> renderForm state p
              Success p -> renderReadOnly p
          ]
      ]
  ]

renderForm :: forall slots m. State -> Maybe Profile -> H.ComponentHTML Action slots m
renderForm state p = HH.form
  [ HE.onSubmit SaveProfile
  , HP.class_ (H.ClassName "flex flex-col gap-2")
  ]
  [ HH.fieldset [ HP.class_ (H.ClassName "fieldset") ]
      [ HH.legend [ HP.class_ (H.ClassName "fieldset-legend") ] [ HH.text "Email" ]
      , HH.input
          [ HP.type_ InputText
          , HP.disabled true
          , HP.class_ (H.ClassName "input w-full")
          , HP.value $ maybe "" (unwrap <<< _.email) p
          ]
      ]
  , HH.fieldset [ HP.class_ (H.ClassName "fieldset") ]
      [ HH.legend [ HP.class_ (H.ClassName "fieldset-legend") ] [ HH.text "Username" ]
      , HH.input
          [ HP.type_ InputText
          , HP.class_ (H.ClassName "input w-full")
          , HP.placeholder "Username"
          , HP.value state.username
          , HE.onValueInput UpdateUsername
          ]
      ]
  , HH.div [ HP.class_ (H.ClassName "grid grid-cols-1 sm:grid-cols-2 gap-2") ]
      [ HH.fieldset [ HP.class_ (H.ClassName "fieldset") ]
          [ HH.legend [ HP.class_ (H.ClassName "fieldset-legend") ] [ HH.text "First Name" ]
          , HH.input
              [ HP.type_ InputText
              , HP.class_ (H.ClassName "input w-full")
              , HP.placeholder "First Name"
              , HP.value state.firstName
              , HE.onValueInput UpdateFirstName
              ]
          ]
      , HH.fieldset [ HP.class_ (H.ClassName "fieldset") ]
          [ HH.legend [ HP.class_ (H.ClassName "fieldset-legend") ] [ HH.text "Last Name" ]
          , HH.input
              [ HP.type_ InputText
              , HP.class_ (H.ClassName "input w-full")
              , HP.placeholder "Last Name"
              , HP.value state.lastName
              , HE.onValueInput UpdateLastName
              ]
          ]
      ]
  , HH.div [ HP.class_ (H.ClassName "card-actions justify-end mt-4") ]
      [ HH.button [ HP.class_ (H.ClassName "btn btn-primary") ]
          [ HH.text "Save" ]
      ]
  ]

renderReadOnly :: forall action slots m. Maybe Profile -> H.ComponentHTML action slots m
renderReadOnly Nothing = HH.div [ HP.class_ (H.ClassName "text-base-content/60 italic py-4") ]
  [ HH.text "This user hasn't set up a profile yet." ]
renderReadOnly (Just profile) = HH.dl [ HP.class_ (H.ClassName "flex flex-col gap-3") ]
  [ field "Username" profile.username
  , field "First Name" profile.firstName
  , field "Last Name" profile.lastName
  ]
  where
  field :: String -> String -> H.ComponentHTML action slots m
  field label value = HH.div [ HP.class_ (H.ClassName "flex flex-col") ]
    [ HH.dt [ HP.class_ (H.ClassName "text-sm text-base-content/60") ] [ HH.text label ]
    , HH.dd [ HP.class_ (H.ClassName "text-base") ]
        [ HH.text (if value == "" then "—" else value) ]
    ]