module Component.Home where

import Prelude

import Component.GameSearch (Size(..))
import Component.GameSearch as GameSearch
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Route (Route(..), navigate)
import Store as S
import Type.Proxy (Proxy(..))

type Slots = (search :: forall query. H.Slot query GameSearch.Output Int)
_search = Proxy :: Proxy "search"

type State = {}

data Action = GameSearchOutput GameSearch.Output

component :: forall query input output m. MonadAff m => MonadEffect m => MonadStore S.Action S.Store m => H.Component query input output m
component = H.mkComponent
  { initialState
  , eval: H.mkEval H.defaultEval { handleAction = handleAction }
  , render
  }

initialState :: forall input. input -> State
initialState _ = {}

handleAction :: forall output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action Slots output m Unit
handleAction (GameSearchOutput (GameSearch.GameSelected bg)) = navigate (GameR bg.bggId)

render :: forall m. MonadAff m => MonadEffect m => State -> H.ComponentHTML Action Slots m
render _ = HH.div [ HP.class_ (H.ClassName "pt-24 flex flex-col gap-6") ]
  [ HH.div [ HP.class_ (H.ClassName "flex gap-4 justify-center") ]
      [ HH.div [ HP.class_ (H.ClassName "flex flex-col gap-2") ]
          [ HH.h1 [ HP.class_ (H.ClassName "text-3xl uppercase font-bold") ] [ HH.text "Never struggle with the box again." ]
          , HH.div_
              [ HH.p_ [ HH.text "Others have felt your pain.  How to put the game back in the box?" ]
              , HH.p_ [ HH.text "Find and share visual guides on how to repack the game effectively and efficiently." ]
              ]
          , HH.div [ HP.class_ (H.ClassName "flex gap-2 items-center") ]
              [ HH.button [ HP.class_ (H.ClassName "btn btn-secondary") ]
                  [ HH.text "Get Started" ]
              ]
          ]
      , HH.div_
          [ HH.img
              [ HP.class_ (H.ClassName "h-64 rounded-xl drop-shadow-xl")
              , HP.src "home-top-right.png"
              ]
          ]
      ]
  , HH.div [ HP.class_ (H.ClassName "p-10") ]
      [ HH.slot _search 0 GameSearch.component Large GameSearchOutput
      ]
  ]
