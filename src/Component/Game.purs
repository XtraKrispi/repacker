module Component.Game where

import Prelude

import Bgg (bggThing)
import Component.ConfirmationButton as ConfirmationButton
import Component.Helpers (addToast)
import Data.Array (length)
import Data.Either (either)
import Data.Filterable (filter)
import Data.Maybe (Maybe(..), maybe)
import Database.Instructions (deleteInstructions, fetchInstructions)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Halogen (get, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SP
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(..))
import Halogen.Svg.Attributes.StrokeLineJoin (StrokeLineJoin(..))
import Halogen.Svg.Elements as Svg
import Network.RemoteData (RemoteData(..), fromEither)
import Route (Route(..), routeCodec)
import Routing.Duplex (print)
import Store as S
import Supabase (Client)
import Supabase.Auth.Types (UserId)
import Type.Proxy (Proxy(..))
import Types (BoardGame, GameId, InstructionsWithUser, SessionInfo, InstructionsKey)

type Slots = (deleteModal :: forall query. H.Slot query ConfirmationButton.Output InstructionsKey)

_deleteModel = Proxy :: Proxy "deleteModal"

type CoreData = (gameId :: GameId, client :: Client, session :: Maybe SessionInfo)
type State =
  { game :: RemoteData String BoardGame
  , instructions :: RemoteData String (Array InstructionsWithUser)
  | CoreData
  }

type Input = { | CoreData }

data Action = Initialize | DeleteInstructions InstructionsKey

component :: forall query output m. MonadEffect m => MonadAff m => MonadStore S.Action S.Store m => H.Component query Input output m
component = H.mkComponent
  { initialState
  , eval: H.mkEval H.defaultEval { initialize = Just Initialize, handleAction = handleAction }
  , render
  }

initialState :: Input -> State
initialState { gameId, client, session } = { gameId, client, session, game: NotAsked, instructions: NotAsked }

handleAction :: forall output m. MonadEffect m => MonadAff m => MonadStore S.Action S.Store m => Action -> H.HalogenM State Action Slots output m Unit
handleAction Initialize = do
  { gameId, client, session } <- get
  modify_ _ { game = Loading, instructions = Loading }
  bg <- liftAff $ bggThing gameId
  instructions <- liftAff $ fetchInstructions client (_.userId <$> session) gameId
  modify_ _ { game = fromEither bg, instructions = Success instructions }
handleAction (DeleteInstructions key) = do
  { client, instructions } <- get
  results <- liftAff $ deleteInstructions client key
  either
    (const $ addToast { message: "There was a problem deleting the instructions, please try again.", severity: S.Error })
    (const $ modify_ _ { instructions = (filter (\s -> s.key /= key)) <$> instructions })
    results

render :: forall m. MonadEffect m => MonadAff m => State -> H.ComponentHTML Action Slots m
render { gameId, game, instructions, session } = HH.div [ HP.class_ (H.ClassName "flex flex-col gap-4") ]
  [ HH.div [] [ renderGameDetails game ]
  , HH.div [ HP.class_ (H.ClassName "divider") ] []
  , HH.div [] [ renderInstructions gameId session instructions ]
  ]

renderGameDetails :: forall action slots m. MonadAff m => MonadEffect m => RemoteData String BoardGame -> H.ComponentHTML action slots m
renderGameDetails (Success game) = HH.div
  [ HP.class_ (H.ClassName "hero min-h-72 rounded-2xl drop-shadow-xl")
  , HP.style ("background-image:url('" <> game.imageUrl <> "')")
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
renderGameDetails Loading = HH.div [ HP.class_ (H.ClassName "hero min-h-72 rounded-2xl bg-base-200") ]
  [ HH.div [ HP.class_ (H.ClassName "hero-content text-center flex flex-col gap-4") ]
      [ HH.span [ HP.class_ (H.ClassName "loading loading-spinner loading-lg text-primary") ] []
      , HH.p [ HP.class_ (H.ClassName "text-base-content/70") ] [ HH.text "Loading game..." ]
      ]
  ]
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

renderInstructions :: forall m. MonadAff m => MonadEffect m => GameId -> Maybe SessionInfo -> RemoteData String (Array InstructionsWithUser) -> HH.ComponentHTML Action Slots m
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
renderInstructions gameId mUser (Success xs) = HH.div [ HP.class_ (H.ClassName "flex flex-col gap-4") ]
  [ HH.div [ HP.class_ (H.ClassName "flex justify-between items-center") ]
      [ HH.h2 [ HP.class_ (H.ClassName "text-2xl font-bold text-primary") ]
          [ HH.text "Packing Guides" ]
      , case mUser of
          Just _ -> HH.a
            [ HP.class_ (H.ClassName "btn btn-primary")
            , HP.href ("#" <> print routeCodec (NewInstructionsR gameId))
            ]
            [ HH.text "+ Add Your Own" ]
          Nothing -> HH.text ""
      ]
  , HH.div [ HP.class_ (H.ClassName "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4") ]
      $ renderInstructionCard gameId (_.userId <$> mUser) <$> xs
  ]
renderInstructions _ _ Loading = HH.div [ HP.class_ (H.ClassName "flex justify-center items-center flex-col gap-4 py-16 text-base-content/50") ]
  [ HH.span [ HP.class_ (H.ClassName "loading loading-spinner loading-lg") ] []
  , HH.p [ HP.class_ (H.ClassName "text-lg") ] [ HH.text "Loading packing guides..." ]
  ]
renderInstructions _ _ _ = HH.div [ HP.class_ (H.ClassName "flex justify-center items-center flex-col gap-4 py-16 text-base-content/50") ] []

renderInstructionCard :: forall m. MonadAff m => MonadEffect m => GameId -> Maybe UserId -> InstructionsWithUser -> HH.ComponentHTML Action Slots m
renderInstructionCard gameId mViewerId { createdBy, key, instructions, isPrivate } =
  let
    isOwner = mViewerId == Just createdBy
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
    deleteBtn = HH.slot _deleteModel key ConfirmationButton.component { buttonText: "Delete", buttonCss: H.ClassName "btn btn-sm btn-error", modalContent: "Are you sure you want to delete these instructions? The operation cannot be undone." } (\_ -> DeleteInstructions key)
  in
    HH.div
      [ HP.class_ (H.ClassName "card bg-base-200 shadow-xl") ]
      [ HH.div [ HP.class_ (H.ClassName "card-body") ]
          [ HH.div [ HP.class_ (H.ClassName "flex justify-between") ]
              [ HH.h3 [ HP.class_ (H.ClassName "card-title text-secondary") ]
                  [ HH.text instructions.description ]
              , if isPrivate then
                  HH.i
                    [ HP.title "Only visible to me"
                    , HP.class_ (H.ClassName "text-secondary")
                    ]
                    [ lockIcon ]
                else
                  HH.i
                    [ HP.title "Visible to everyone"
                    , HP.class_ (H.ClassName "text-secondary")
                    ]
                    [ unlockedIcon ]
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
              $ if isOwner then [ editBtn, viewBtn, deleteBtn ] else [ viewBtn ]
          ]
      ]

lockIcon ∷ ∀ (p127 ∷ Type) (i128 ∷ Type). HH.HTML p127 i128
lockIcon = Svg.svg
  [ SP.class_ (H.ClassName "size-6")
  , SP.fill NoColor
  , SP.viewBox 0.0 0.0 24.0 24.0
  , SP.stroke (Named "currentColor")
  ]
  [ Svg.path
      [ SP.strokeLineCap LineCapRound
      , SP.strokeLineJoin LineJoinRound
      , SP.strokeWidth 1.5
      , SP.d
          [ SP.m SP.Abs 16.5 10.5
          , SP.v SP.Abs 6.75
          , SP.a SP.Rel 4.5 4.5 0.0 SP.Arc1 SP.Sweep0 (-9.0) 0.0
          , SP.v SP.Rel 3.75
          , SP.m SP.Rel (-0.75) 11.25
          , SP.h SP.Rel 10.5
          , SP.a SP.Rel 2.25 2.25 0.0 SP.Arc0 SP.Sweep0 2.25 (-2.25)
          , SP.v SP.Rel (-6.75)
          , SP.a SP.Rel 2.25 2.25 0.0 SP.Arc0 SP.Sweep0 (-2.25) (-2.25)
          , SP.h SP.Abs 6.75
          , SP.a SP.Rel 2.25 2.25 0.0 SP.Arc0 SP.Sweep0 (-2.25) 2.25
          , SP.v SP.Rel 6.75
          , SP.a SP.Rel 2.25 2.25 0.0 SP.Arc0 SP.Sweep0 2.25 2.25
          , SP.z
          ]
      ]
  ]

unlockedIcon ∷ ∀ (p127 ∷ Type) (i128 ∷ Type). HH.HTML p127 i128
unlockedIcon = Svg.svg
  [ SP.class_ (H.ClassName "size-6")
  , SP.fill NoColor
  , SP.viewBox 0.0 0.0 24.0 24.0
  , SP.stroke (Named "currentColor")
  ]
  [ Svg.path
      [ SP.strokeLineCap LineCapRound
      , SP.strokeLineJoin LineJoinRound
      , SP.strokeWidth 1.5
      , SP.d
          [ SP.m SP.Abs 13.5 10.5
          , SP.v SP.Abs 6.75
          , SP.a SP.Rel 4.5 4.5 0.0 SP.Arc1 SP.Sweep1 9.0 0.0
          , SP.v SP.Rel 3.75
          , SP.m SP.Abs 3.75 21.75
          , SP.h SP.Rel 10.5
          , SP.a SP.Rel 2.25 2.25 0.0 SP.Arc0 SP.Sweep0 2.25 (-2.25)
          , SP.v SP.Rel (-6.75)
          , SP.a SP.Rel 2.25 2.25 0.0 SP.Arc0 SP.Sweep0 (-2.25) (-2.25)
          , SP.h SP.Abs 3.75
          , SP.a SP.Rel 2.25 2.25 0.0 SP.Arc0 SP.Sweep0 (-2.25) 2.25
          , SP.v SP.Rel 6.75
          , SP.a SP.Rel 2.25 2.25 0.0 SP.Arc0 SP.Sweep0 2.25 2.25
          , SP.z
          ]
      ]
  ]