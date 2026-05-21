module Component.Game where

import Prelude

import Bgg (bggThing)
import Data.Maybe (Maybe(..), maybe)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Halogen (get, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SP
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(..))
import Halogen.Svg.Attributes.StrokeLineJoin (StrokeLineJoin(..))
import Halogen.Svg.Elements as Svg
import Network.RemoteData (RemoteData(..), fromEither)
import Types (GameId, BoardGame)

type State = { gameId :: GameId, game :: RemoteData String BoardGame }

data Action = Initialize

component :: forall query output m. MonadEffect m => MonadAff m => H.Component query GameId output m
component = H.mkComponent
  { initialState
  , eval: H.mkEval H.defaultEval { initialize = Just Initialize, handleAction = handleAction }
  , render
  }

initialState :: GameId -> State
initialState gameId = { gameId, game: NotAsked }

handleAction :: forall slots output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action slots output m Unit
handleAction Initialize = do
  { gameId } <- get
  modify_ _ { game = Loading }
  bg <- liftAff $ bggThing gameId
  modify_ _ { game = fromEither bg }

render :: forall action slots m. MonadEffect m => MonadAff m => State -> H.ComponentHTML action slots m
render { game } = HH.div [ HP.class_ (H.ClassName "flex flex-col gap-4") ]
  [ HH.div [] [ renderGameDetails game ]
  , HH.div [ HP.class_ (H.ClassName "divider") ] []
  , HH.div [] [ HH.text "Instructions listing" ]
  ]

renderGameDetails :: forall action slots m. MonadAff m => MonadEffect m => RemoteData String BoardGame -> H.ComponentHTML action slots m
renderGameDetails (Success game) = HH.div
  [ HP.class_ (H.ClassName "hero min-h-72 rounded-2xl drop-shadow-xl")
  , HP.style ("background-image: url('" <> game.imageUrl <> "')")
  ]
  [ HH.div [ HP.class_ (H.ClassName "hero-overlay rounded-2xl") ] []
  , HH.div [ HP.class_ (H.ClassName "hero-content text-neutral-content text-center") ]
      [ HH.div [ HP.class_ (H.ClassName "max-w-full") ]
          [ HH.h1 [ HP.class_ (H.ClassName "mb-5 text-5xl font-bold") ]
              [ HH.text game.title
              , maybe (HH.text "") (\y -> HH.text (" (" <> show y <> ")")) game.yearPublished
              ]
          ]
      ]
  ]
renderGameDetails Loading = HH.div_ [ HH.text "Loading..." ]
renderGameDetails (Failure err) = HH.div [ HP.class_ (H.ClassName "alert alert-error") ]
  [ Svg.svg
      [ SP.class_ (H.ClassName "h-6 w-6 shrink-0 stroke-current")
      , SP.fill NoColor
      , SP.viewBox 0.0 0.0 24.0 24.0
      ]
      [ Svg.path
          [ SP.strokeLineCap LineCapRound
          , SP.strokeLineJoin LineJoinRound
          , SP.strokeWidth 2.0
          , SP.d
              [ SP.m SP.Abs 10.0 14.0
              , SP.l SP.Rel 2.0 (-2.0)
              , SP.m SP.Rel 0.0 0.0
              , SP.l SP.Rel 2.0 (-2.0)
              , SP.m SP.Rel (-2.0) 2.0
              , SP.l SP.Rel (-2.0) (-2.0)
              , SP.m SP.Rel 2.0 2.0
              , SP.l SP.Rel 2.0 2.0
              , SP.m SP.Rel 7.0 (-2.0)
              , SP.a SP.Rel 9.0 9.0 0.0 SP.Arc1 SP.Sweep1 (-18.0) 0.0
              , SP.a SP.Rel 9.0 9.0 0.0 SP.Arc0 SP.Sweep1 18.0 0.0
              , SP.z
              ]
          ]
      ]
  , HH.span_ [ HH.text err ]
  ]
renderGameDetails NotAsked = HH.div [] []
