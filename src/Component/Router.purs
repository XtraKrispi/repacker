module Component.Router where

import Prelude

import Component.Game as Game
import Component.Home as Home
import Component.Instructions as Instructions
import Component.MySubmissions as MySubmissions
import Component.Navbar as Navbar
import Component.Profile as Profile
import Component.Toast as Toast
import Component.ViewInstructions as ViewInstructions
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.UUID (toString)
import Data.UUID as UUID
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console (log)
import Halogen (gets, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore)
import Route (Route(..), navigate)
import Store (selectSession)
import Store as S
import Supabase (Client)
import Type.Proxy (Proxy(..))
import Types (SessionInfo)

type Slots =
  ( page :: forall query. H.Slot query Void String
  , navbar :: forall query. H.Slot query Void Int
  , toasts :: forall query. H.Slot query Void Int
  )

_page = Proxy :: Proxy "page"
_navbar = Proxy :: Proxy "navbar"
_toasts = Proxy :: Proxy "toasts"

type Input =
  { initialRoute :: Route
  , client :: Client
  }

type State =
  { currentRoute :: Route
  , session :: Maybe SessionInfo
  , client :: Client
  }

data Query a = ChangeRoute Route a

data Action = UpdateState (Maybe SessionInfo) Input

component
  :: forall output m
   . MonadAff m
  => MonadEffect m
  => MonadStore S.Action S.Store m
  => H.Component Query Input output m
component = connect selectSession $ H.mkComponent
  { initialState
  , eval: H.mkEval H.defaultEval
      { handleQuery = handleQuery
      , handleAction = handleAction
      , receive = receive
      }
  , render
  }

receive :: Connected (Maybe SessionInfo) Input -> Maybe Action
receive { context, input } = Just $ UpdateState context input

initialState :: Connected (Maybe SessionInfo) Input -> State
initialState { context, input: { initialRoute, client } } =
  { currentRoute: initialRoute
  , session: context
  , client
  }

handleQuery :: forall a action output m. MonadAff m => MonadEffect m => Query a -> H.HalogenM State action Slots output m (Maybe a)
handleQuery (ChangeRoute route a) = do
  modify_ _ { currentRoute = route }
  liftEffect $ log "I've changed the route!"
  pure (Just a)

handleAction :: forall output m. MonadAff m => MonadEffect m => MonadStore S.Action S.Store m => Action -> H.HalogenM State Action Slots output m Unit
handleAction (UpdateState session _) = do
  route <- gets _.currentRoute
  modify_
    _
      { session = session
      }
  navigateIfNecessary session route

navigateIfNecessary :: forall output m. MonadAff m => MonadEffect m => MonadStore S.Action S.Store m => Maybe SessionInfo -> Route -> H.HalogenM State Action Slots output m Unit
navigateIfNecessary Nothing (NewInstructionsR gameId) = navigate (GameR gameId)
navigateIfNecessary Nothing (UpdateInstructionsR gameId instructionsKey) = navigate (ViewInstructionsR gameId instructionsKey)
navigateIfNecessary _ _ = pure unit

render :: forall action m. MonadAff m => MonadEffect m => MonadStore S.Action S.Store m => State -> H.ComponentHTML action Slots m
render state = HH.div []
  [ HH.slot_ _navbar 0 Navbar.component
      { currentRoute: state.currentRoute
      , client: state.client
      }
  , HH.div [ HP.class_ (H.ClassName "px-32 pt-4") ]
      [ case state.currentRoute of
          HomeR -> HH.slot_ _page "home" Home.component unit
          GameR gameId -> HH.slot_ _page ("game " <> (unwrap gameId)) Game.component { gameId: gameId, client: state.client, session: state.session }
          ProfileR userId -> HH.slot_ _page ("profile " <> (UUID.toString $ unwrap (unwrap userId))) Profile.component { client: state.client, userId }
          NewInstructionsR gameId -> case state.session of
            Just s -> HH.slot_ _page ("new-instructions " <> unwrap gameId) Instructions.component { client: state.client, gameId, session: s, existingKey: Nothing }
            Nothing -> HH.div [] []
          UpdateInstructionsR gameId instructionsKey ->
            case state.session of
              Just s -> HH.slot_ _page ("update-instructions " <> unwrap gameId <> (toString (unwrap instructionsKey))) Instructions.component { client: state.client, gameId, session: s, existingKey: Just instructionsKey }
              Nothing -> HH.div [] []
          MySubmissionsR ->
            case state.session of
              Just s -> HH.slot_ _page "my-submissions" MySubmissions.component { client: state.client, session: s }
              Nothing -> HH.div [] []
          ViewInstructionsR gameId instructionsKey ->
            HH.slot_ _page ("view-instructions " <> unwrap gameId <> (toString (unwrap instructionsKey))) ViewInstructions.component { client: state.client, gameId, instructionsKey, session: state.session }
      ]
  , HH.slot_ _toasts 0 Toast.component unit
  ]
