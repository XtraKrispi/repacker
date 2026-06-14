module Component.MySubmissions where

import Prelude

import Bgg (bggThings)
import Data.Array (catMaybes, find, length, nub)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Database.Instructions (fetchUserInstructions)
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
import Network.RemoteData (RemoteData(..))
import Route (Route(..), routeCodec)
import Routing.Duplex (print)
import Supabase (Client)
import Types (BoardGame, GameId, InstructionsResult, SessionInfo)

-- TODO: Plumb this through

type CoreData = (client :: Client, session :: SessionInfo)

type Input = { | CoreData }

type RawInstructionsData = { gameId :: GameId, game :: BoardGame | InstructionsResult }

type InstructionsData = { gameId :: GameId, game :: BoardGame, instructions :: Array { | InstructionsResult } }

type State = { instructions :: RemoteData String (Array InstructionsData) | CoreData }

data Action = Initialize

-- TODO: Add paging here (max page size = 20) -> Page based on board games, not instructions

component :: forall query output m. MonadEffect m => MonadAff m => H.Component query Input output m
component = H.mkComponent
  { initialState
  , eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
  , render
  }

initialState :: Input -> State
initialState { client, session } = { client, session, instructions: NotAsked }

handleAction :: forall slots output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action slots output m Unit
handleAction Initialize = do
  state <- get
  modify_ _ { instructions = Loading }
  results <- liftAff $ fetchUserInstructions state.client state.session.userId
  let gameIds = nub $ _.gameId <$> results
  things <- liftAff $ bggThings gameIds
  case things of
    Left err -> modify_ _ { instructions = Failure err }
    Right t -> do
      let
        instructionsWithGame =
          catMaybes $ (\{ gameId, key, instructions } -> (\game -> { gameId, game, key, instructions }) <$> find (\thing -> thing.bggId == gameId) t) <$> results
      modify_ _ { instructions = Success (extractData $ buildMap instructionsWithGame) }
  where
  buildMap :: Array RawInstructionsData -> Map { gameId :: GameId, game :: BoardGame } (Array { | InstructionsResult })
  buildMap d = foldr (\{ gameId, game, key, instructions } -> Map.insertWith (<>) { gameId, game } [ { key, instructions } ]) Map.empty d

  extractData :: Map { gameId :: GameId, game :: BoardGame } (Array { | InstructionsResult }) -> Array InstructionsData
  extractData m = (\(Tuple { gameId, game } v) -> { gameId, game, instructions: v }) <$> Map.toUnfoldable m

render :: forall slots m. MonadEffect m => MonadAff m => State -> H.ComponentHTML Action slots m
render state =
  HH.div [ HP.class_ (H.ClassName "max-w-md mx-auto w-full") ]
    [ renderInstructions state.instructions ]

renderInstructions :: forall action slots m. MonadAff m => MonadEffect m => RemoteData String (Array InstructionsData) -> HH.ComponentHTML action slots m
renderInstructions (Success []) = HH.div [ HP.class_ (H.ClassName "flex justify-center items-center flex-col gap-4 py-16 text-base-content/50") ]
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
  , HH.p [ HP.class_ (H.ClassName "text-lg") ] [ HH.text "You haven't submitted any instructions yet" ]
  ]
renderInstructions (Success xs) = HH.div [ HP.class_ (H.ClassName "flex flex-col gap-4") ]
  [ HH.div [ HP.class_ (H.ClassName "flex justify-between items-center") ]
      [ HH.h2 [ HP.class_ (H.ClassName "text-2xl font-bold text-primary") ]
          [ HH.text "My Packing Guides" ]
      ]
  , HH.div [ HP.class_ (H.ClassName "flex flex-col gap-4") ]
      $ renderGameCard <$> xs
  ]
renderInstructions Loading = HH.div [ HP.class_ (H.ClassName "flex justify-center items-center flex-col gap-4 py-16 text-base-content/50") ]
  [ HH.span [ HP.class_ (H.ClassName "loading loading-spinner loading-lg") ] []
  , HH.p [ HP.class_ (H.ClassName "text-lg") ] [ HH.text "Loading packing guides..." ]
  ]
renderInstructions _ = HH.div [ HP.class_ (H.ClassName "flex justify-center items-center flex-col gap-4 py-16 text-base-content/50") ] []

renderGameCard :: forall action slots m. MonadAff m => MonadEffect m => InstructionsData -> HH.ComponentHTML action slots m
renderGameCard { gameId, game, instructions } = HH.div [ HP.class_ (H.ClassName "card bg-base-200 shadow-xl") ]
  [ HH.div [ HP.class_ (H.ClassName "card-body") ]
      [ HH.div [ HP.class_ (H.ClassName "flex gap-4 text-2xl items-center card-title ") ]
          [ HH.img [ HP.class_ (H.ClassName "rounded-full h-12 w-12"), HP.src game.thumbnailUrl ]
          , HH.span [] [ HH.text $ game.title <> maybe "" (\y -> " (" <> show y <> ")") game.yearPublished ]
          ]
      , HH.div [ HP.class_ (H.ClassName "divider") ] []
      , HH.div [] $ renderInstructionCard gameId <$> instructions
      ]

  ]

renderInstructionCard :: forall action slots m. MonadAff m => MonadEffect m => GameId -> { | InstructionsResult } -> HH.ComponentHTML action slots m
renderInstructionCard gameId { key, instructions } =
  let
    viewBtn = HH.a
      [ HP.class_ (H.ClassName "btn btn-sm btn-primary")
      , HP.href ("#" <> print routeCodec (ViewInstructionsR gameId key))
      ]
      [ HH.text "View" ]
    editBtn = HH.a
      [ HP.class_ (H.ClassName "btn btn-sm btn-secondary")
      , HP.href ("#" <> print routeCodec (UpdateInstructionsR gameId key))
      ]
      [ HH.text "Edit" ]
  in
    HH.div
      []
      [ HH.div []
          [ HH.h3 [ HP.class_ (H.ClassName "card-title text-secondary") ]
              [ HH.div [ HP.class_ (H.ClassName "flex flex-col gap-2 w-full") ]
                  [ HH.span [] [ HH.text instructions.description ]
                  ]
              ]
          , HH.div [ HP.class_ (H.ClassName "flex gap-2 flex-wrap mt-2") ]
              [ HH.span [ HP.class_ (H.ClassName "badge badge-ghost") ]
                  [ HH.text $ show (length instructions.steps) <> " step" <> if length instructions.steps == 1 then "" else "s" ]
              , if instructions.allowsSleeves then
                  HH.span [ HP.class_ (H.ClassName "badge badge-info") ] [ HH.text "Sleeves" ]
                else HH.text ""
              , if instructions.requiresBaggies then
                  HH.span [ HP.class_ (H.ClassName "badge badge-info") ] [ HH.text "Baggies" ]
                else HH.text ""
              ]
          , HH.div [ HP.class_ (H.ClassName "card-actions justify-end mt-4") ]
              [ editBtn, viewBtn ]
          ]
      ]
