module Component.GameSearch where

import Prelude

import Bgg (bggSearch)
import Data.Array (null)
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
  , loading :: Boolean
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
  , loading: false
  , searchResults: NotAsked
  }

handleAction :: forall slots m. MonadAff m => MonadEffect m => Action -> H.HalogenM State Action slots Output m Unit
handleAction (SearchTextChanged "") = do
  { debouncer } <- get
  case debouncer of
    Just d -> H.kill d
    Nothing -> pure unit
  modify_ _ { search = "", debouncer = Nothing }
handleAction (SearchTextChanged str) = do
  { debouncer } <- get
  modify_ _ { search = str }
  case debouncer of
    Just d -> H.kill d
    Nothing -> pure unit
  forkId <- H.fork do
    liftAff $ delay (Milliseconds 400.0)
    modify_ _ { loading = true }
    liftEffect $ openModal "#results-modal"
    searchResults <- liftAff $ bggSearch str
    modify_ _ { loading = false, searchResults = fromEither searchResults }
  modify_ _ { debouncer = Just forkId }
handleAction (SelectGame bg) = do
  { size } <- get
  H.raise (GameSelected bg)
  liftEffect $ close "#results-modal"
  put (initialState size)

render :: forall slots m. MonadAff m => MonadEffect m => State -> H.ComponentHTML Action slots m
render { size, searchResults, search, loading } = HH.div
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
      [ HH.div [ HP.class_ (H.ClassName "modal-box w-10/12 max-w-5xl max-h-[75vh] flex flex-col") ]
          [ HH.form [ HP.method Dialog ] [ HH.button [ HP.class_ (H.ClassName "btn btn-sm btn-circle btn-ghost absolute right-2 top-2") ] [ HH.text "✕" ] ]
          , HH.h3 [ HP.class_ (H.ClassName "text-lg font-bold shrink-0") ] [ HH.text "Search Results" ]
          , HH.label
              [ HP.class_ (H.ClassName "input w-full my-4 shrink-0") ]
              [ HH.input
                  [ HP.type_ InputSearch
                  , HP.class_ (H.ClassName "grow")
                  , HP.placeholder "Search for a game..."
                  , HE.onValueInput SearchTextChanged
                  , HP.value search
                  ]
              , if loading then HH.span [ HP.class_ (H.ClassName "loading loading-spinner loading-sm") ] []
                else HH.text ""
              ]
          , HH.div [ HP.class_ (H.ClassName "flex-1 overflow-y-auto min-h-0") ]
              [ case searchResults of
                  Loading -> HH.text ""
                  Success results
                    | null results -> HH.div [ HP.class_ (H.ClassName "py-8 text-center") ]
                        [ HH.p [ HP.class_ (H.ClassName "text-base-content/60") ]
                            [ HH.text $ "No games found matching \"" <> search <> "\"" ]
                        , HH.p [ HP.class_ (H.ClassName "text-sm text-base-content/40 mt-2") ]
                            [ HH.text "Try a different search term." ]
                        ]
                    | otherwise -> HH.table [ HP.class_ (H.ClassName "table") ]
                        ( ( \bg ->
                              HH.tr [ HE.onClick (\_ -> SelectGame bg), HP.class_ (H.ClassName "cursor-pointer hover:bg-base-200 list-row") ]
                                [ HH.td_ [ HH.text $ maybe "N/A" show bg.yearPublished ]
                                , HH.td_ [ HH.text bg.title ]
                                ]
                          ) <$> results
                        )
                  Failure err -> HH.div [ HP.class_ (H.ClassName "py-8 text-center text-error") ]
                    [ HH.text $ "Something went wrong: " <> err ]
                  NotAsked -> HH.text ""
              ]
          ]
      , HH.form
          [ HP.method Dialog
          , HP.class_ (H.ClassName "modal-backdrop")
          ]
          [ HH.button_ [ HH.text "close" ] ]
      ]
  ]

