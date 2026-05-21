module Component.GameSearch where

import Prelude

import Bgg (bggSearch)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import FFI.Dialog (close, openModal)
import Halogen (get, modify_, put)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (FormMethod(..), InputType(..))
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..), fromEither)
import Types (BoardGameSummary)

data Size
  = Large
  | Regular

type State =
  { size :: Size
  , debouncer :: Maybe H.ForkId
  , search :: String
  , searchResults :: RemoteData String (Array BoardGameSummary)
  }

data Action
  = SearchTextChanged String
  | SelectGame BoardGameSummary

data Output = GameSelected BoardGameSummary

component :: forall query m. MonadAff m => MonadEffect m => H.Component query Size Output m
component = H.mkComponent { initialState, eval: H.mkEval H.defaultEval { handleAction = handleAction }, render }

initialState :: Size -> State
initialState size =
  { size
  , debouncer: Nothing
  , search: ""
  , searchResults: NotAsked
  }

handleAction :: forall slots m. MonadAff m => MonadEffect m => Action -> H.HalogenM State Action slots Output m Unit
handleAction (SearchTextChanged str) = do
  { debouncer } <- get
  modify_ _ { search = str }
  case debouncer of
    Just d -> H.kill d
    Nothing -> pure unit
  forkId <- H.fork do
    liftAff $ delay (Milliseconds 400.0)
    searchResults <- liftAff $ bggSearch str
    modify_ _ { searchResults = fromEither searchResults }
    liftEffect $ openModal "#results-modal"
  modify_ _ { debouncer = Just forkId }
handleAction (SelectGame bg) = do
  { size } <- get
  H.raise (GameSelected bg)
  liftEffect $ close "#results-modal"
  put (initialState size)

render :: forall slots m. MonadAff m => MonadEffect m => State -> H.ComponentHTML Action slots m
render { size, searchResults, search } = HH.div
  [ HP.class_ (H.ClassName "w-full block") ]
  [ HH.input
      [ HP.type_ InputSearch
      , HP.class_
          ( case size of
              Large -> (H.ClassName "input input-xl w-full")
              Regular -> (H.ClassName "input ")
          )
      , HP.placeholder "Search for a game..."
      , HE.onValueInput SearchTextChanged
      , HP.value search
      ]
  , HH.dialog
      [ HP.id "results-modal"
      , HP.class_ (H.ClassName "modal")
      ]
      [ HH.div [ HP.class_ (H.ClassName "modal-box w-10/12 max-w-5xl") ]
          [ HH.form [ HP.method Dialog ] [ HH.button [ HP.class_ (H.ClassName "btn btn-sm btn-circle btn-ghost absolute right-2 top-2") ] [ HH.text "✕" ] ]
          , HH.h3 [ HP.class_ (H.ClassName "text-lg font-bold") ] [ HH.text "Search Results" ]
          , case searchResults of
              Success results -> HH.table [ HP.class_ (H.ClassName "py-4 table max-h-[50vh] overflow-scroll") ]
                ( ( \bg ->
                      HH.tr [ HE.onClick (\_ -> SelectGame bg), HP.class_ (H.ClassName "cursor-pointer hover:bg-base-200 list-row") ]
                        [ HH.td_ [ HH.text $ maybe "N/A" show bg.yearPublished ]
                        , HH.td_ [ HH.text bg.title ]
                        ]
                  ) <$> results
                )
              _ -> HH.p_ [ HH.text "Something is up" ]
          ]
      , HH.form
          [ HP.method Dialog
          , HP.class_ (H.ClassName "modal-backdrop")
          ]
          [ HH.button_ [ HH.text "close" ] ]
      ]
  ]

