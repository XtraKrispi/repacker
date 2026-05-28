module Component.Game where

import Prelude

import Bgg (bggThing)
import Data.Maybe (Maybe(..), maybe)
import Data.Profunctor.Split (unSplit)
import Data.Tuple (Tuple)
import Database.Instructions (fetchInstructions)
import Debug (trace)
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
import Route (Route(..), routeCodec)
import Routing.Duplex (print)
import Supabase (Client)
import Supabase.Auth.Types (UserId(..))
import Types (BoardGame, GameId, Instructions, SessionInfo)

type CoreData = (gameId :: GameId, client :: Client, session :: Maybe SessionInfo)
type State =
  { game :: RemoteData String BoardGame
  , instructions :: RemoteData String (Array (Tuple UserId Instructions))
  | CoreData
  }

type Input = { | CoreData }

data Action = Initialize

component :: forall query output m. MonadEffect m => MonadAff m => H.Component query Input output m
component = H.mkComponent
  { initialState
  , eval: H.mkEval H.defaultEval { initialize = Just Initialize, handleAction = handleAction }
  , render
  }

initialState :: Input -> State
initialState { gameId, client, session } = { gameId, client, session, game: NotAsked, instructions: NotAsked }

handleAction :: forall slots output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action slots output m Unit
handleAction Initialize = do
  { gameId, client } <- get
  modify_ _ { game = Loading, instructions = Loading }
  bg <- liftAff $ bggThing gameId
  instructions <- liftAff $ fetchInstructions client gameId
  modify_ _ { game = fromEither bg, instructions = Success instructions }

render :: forall action slots m. MonadEffect m => MonadAff m => State -> H.ComponentHTML action slots m
render { gameId, game, instructions, session } = HH.div [ HP.class_ (H.ClassName "flex flex-col gap-4") ]
  [ HH.div [] [ renderGameDetails game ]
  , HH.div [ HP.class_ (H.ClassName "divider") ] []
  , HH.div [] [ renderInstructions gameId session instructions ]
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

renderInstructions :: forall action slots m. MonadAff m => MonadEffect m => GameId -> Maybe SessionInfo -> RemoteData String (Array (Tuple UserId Instructions)) -> HH.ComponentHTML action slots m
renderInstructions gameId mUser (Success []) = HH.div [ HP.class_ (H.ClassName "flex justify-center items-center flex-col gap-4 py-16 text-base-content/50") ]
  [ Svg.svg
      [ SP.class_ (H.ClassName "h-12 w-12 opacity-40")
      , SP.fill NoColor
      , SP.viewBox 0.0 0.0 24.0 24.0
      , SP.stroke (Named "currentColor")
      ]
      [ Svg.path
          [ SP.strokeLineCap LineCapRound
          , SP.strokeLineJoin LineJoinRound
          , SP.strokeWidth 1.5
          , SP.d
              [ SP.m SP.Abs 9.0 12.0
              , SP.h SP.Rel 6.0
              , SP.m SP.Rel (-6.0) 4.0
              , SP.h SP.Rel 6.0
              , SP.m SP.Rel 2.0 5.0
              , SP.h SP.Abs 7.0
              , SP.a SP.Rel 2.0 2.0 0.0 SP.Arc0 SP.Sweep1 (-2.0) (-2.0)
              , SP.v SP.Abs 5.0
              , SP.a SP.Rel 2.0 2.0 0.0 SP.Arc0 SP.Sweep1 2.0 (-2.0)
              , SP.h SP.Rel 5.586
              , SP.a SP.Rel 1.0 1.0 0.0 SP.Arc0 SP.Sweep1 0.707 0.293
              , SP.l SP.Rel 5.414 5.414
              , SP.a SP.Rel 1.0 1.0 0.0 SP.Arc0 SP.Sweep1 0.293 0.707
              , SP.v SP.Abs 19.0
              , SP.a SP.Rel 2.0 2.0 0.0 SP.Arc0 SP.Sweep1 (-2.0) 2.0
              , SP.z
              ]
          ]
      ]
  , HH.p [ HP.class_ (H.ClassName "text-lg") ] [ HH.text "No packing instructions yet" ]
  , case mUser of
      Just _ -> HH.a
        [ HP.class_ (H.ClassName "btn btn-primary")
        , HP.href ("#" <> print routeCodec (NewInstructionsR gameId))
        ]
        [ HH.text "Be the first to add instructions" ]
      Nothing -> HH.p [ HP.class_ (H.ClassName "text-sm") ]
        [ HH.text "Log in above to add your own"
        ]
  ]
renderInstructions gameId mUser (Success xs) = HH.div [ HP.class_ (H.ClassName "flex justify-center items-center flex-col gap-4 py-16 text-base-content/50") ] []
renderInstructions gameId mUser _ = HH.div [ HP.class_ (H.ClassName "flex justify-center items-center flex-col gap-4 py-16 text-base-content/50") ] []
