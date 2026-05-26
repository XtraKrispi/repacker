module Component.NewInstructions where

import Prelude

import Bgg (bggThing)
import DOM.HTML.Indexed.InputAcceptType (mediaType)
import Data.Array (filter, mapWithIndex, sortWith)
import Data.Foldable (maximum)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType (MediaType(..))
import Data.Newtype (unwrap)
import Data.Set as Set
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Halogen (AttrName(..), get, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..), InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SP
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(..))
import Halogen.Svg.Attributes.StrokeLineJoin (StrokeLineJoin(..))
import Halogen.Svg.Elements as Svg
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteDate
import Supabase (Client, UserId)
import Types (BoardGame, GameId, Instructions, SessionInfo, PackingStep)

type CoreData =
  ( client :: Client
  , gameId :: GameId
  , sessionInfo :: SessionInfo
  )

type State =
  { game :: RemoteData String BoardGame
  , instructions :: Instructions
  | CoreData
  }

type Input = { | CoreData }

defaultInstructions :: UserId -> GameId -> Instructions
defaultInstructions userId gameId =
  { description: ""
  , bggId: gameId
  , creator: userId
  , allowsSleeves: false
  , requiresBaggies: false
  , steps: [ defaultStep 1 ]
  , includedExpansions: Set.empty
  , otherMaterials: []
  , customInsert: Nothing
  }

defaultStep :: Int -> PackingStep
defaultStep ordinal =
  { description: ""
  , imageId: Nothing
  , stepOrdinal: ordinal
  }

data Action
  = Initialize
  | UpdateInstructionsDescription String
  | NewStep
  | ToggleExpansion GameId
  | RemoveStep PackingStep
  | UpdateStepDescription PackingStep String

component :: forall query output m. MonadEffect m => MonadAff m => H.Component query Input output m
component = H.mkComponent
  { initialState
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  , render
  }

initialState :: Input -> State
initialState { client, gameId, sessionInfo } = { client, gameId, sessionInfo, game: NotAsked, instructions: defaultInstructions sessionInfo.userId gameId }

handleAction :: forall slots output m. MonadAff m => MonadEffect m => Action -> H.HalogenM State Action slots output m Unit
handleAction Initialize = do
  { gameId } <- get
  modify_ _ { game = Loading }
  eThing <- liftAff $ bggThing gameId
  modify_ _ { game = RemoteDate.fromEither eThing }
handleAction (UpdateInstructionsDescription str) = modify_ $ \state -> state { instructions = state.instructions { description = str } }
handleAction NewStep = do
  { instructions } <- get
  let nextOrdinal = 1 + fromMaybe 0 (maximum (_.stepOrdinal <$> instructions.steps))
  modify_ $ \state -> state { instructions = state.instructions { steps = state.instructions.steps <> [ defaultStep nextOrdinal ] } }
handleAction (ToggleExpansion gameId) = do
  { instructions } <- get
  let
    new =
      if Set.member gameId instructions.includedExpansions then
        Set.delete gameId instructions.includedExpansions
      else Set.insert gameId instructions.includedExpansions

  modify_ _ { instructions = instructions { includedExpansions = new } }
handleAction (RemoveStep step) = do
  { instructions } <- get
  let filtered = filter (\s -> s /= step) $ sortWith _.stepOrdinal instructions.steps
  let reordered = mapWithIndex (\i s -> s { stepOrdinal = i + 1 }) filtered
  modify_ \state -> state { instructions = state.instructions { steps = reordered } }
handleAction (UpdateStepDescription step str) = do
  let newStep = step { description = str }
  modify_ (\state -> state { instructions = state.instructions { steps = map (\s -> if s == step then newStep else s) state.instructions.steps } })

render :: forall slots m. MonadAff m => MonadEffect m => State -> H.ComponentHTML Action slots m
render state = HH.div [ HP.class_ (H.ClassName "p-8") ]
  [ HH.div_ [ HH.text (show state.instructions) ]
  , HH.div [ HP.class_ (H.ClassName "max-w-4xl mx-auto space-y-6") ]
      [ HH.header [ HP.class_ (H.ClassName "flex justify-between items-center") ]
          [ HH.div []
              [ HH.h1 [ HP.class_ (H.ClassName "text-3xl font-bold text-primary") ]
                  [ HH.text "Create Repack Guide" ]
              , HH.p [ HP.class_ (H.ClassName "text-base-content/70") ] [ HH.text "Instructions for organizational perfection." ]
              ]
          , HH.button [ HP.class_ (H.ClassName "btn btn-primary px-8"), HP.type_ ButtonSubmit, HP.attr (AttrName "form") "instructions-form" ] [ HH.text "Publish Guide" ]
          ]
      , instructionsForm state
      ]
  ]

instructionsForm :: forall slots m. MonadAff m => MonadEffect m => State -> H.ComponentHTML Action slots m
instructionsForm state =
  case state.game of
    Success game -> HH.form [ HP.id "instructions-form" ]
      [ HH.div [ HP.class_ (H.ClassName "grid grid-cols-1 md:grid-cols-3 gap-6") ]
          [ HH.main [ HP.class_ (H.ClassName "md:col-span-2 space-y-6") ]
              [ HH.div [ HP.class_ (H.ClassName "card bg-base-200 shadow-xl") ]
                  [ HH.div [ HP.class_ (H.ClassName "card-body") ]
                      [ HH.h2 [ HP.class_ (H.ClassName "card-title text-secondary") ]
                          [ HH.text "General Information" ]
                      , HH.div [ HP.class_ (H.ClassName "form-control w-full") ]
                          [ HH.label [ HP.class_ (H.ClassName "label") ]
                              [ HH.span [ HP.class_ (H.ClassName "label-text") ] [ HH.text "Title" ]
                              ]
                          , HH.input
                              [ HP.class_ (H.ClassName "input input-bordered w-full")
                              , HP.placeholder "e.g. Scythe Legendary Box Layout"
                              , HP.required true
                              , HE.onValueInput UpdateInstructionsDescription
                              , HP.value state.instructions.description
                              ]
                          ]
                      , HH.div [ HP.class_ (H.ClassName "form-control w-full mt-4") ]
                          [ HH.label [ HP.class_ (H.ClassName "label") ]
                              [ HH.span [ HP.class_ (H.ClassName "label-text") ] [ HH.text "Select Included Expansions" ]
                              , HH.span [ HP.class_ (H.ClassName "label-text-alt text-info") ] [ HH.text "Check all that apply" ]
                              ]
                          , HH.div [ HP.class_ (H.ClassName "border border-base-300 rounded-lg max-h-48 overflow-y-auto p-2 bg-base-50 flex flex-col gap-1") ]
                              $ renderExpansion <$> game.expansions
                          ]
                      ]
                  ]
              , HH.div [ HP.class_ (H.ClassName "space-y-4") ]
                  [ HH.h2 [ HP.class_ (H.ClassName "text-xl font-bold") ]
                      [ HH.text "Packing Steps" ]
                  , HH.div [ HP.class_ (H.ClassName "space-y-4") ] $ renderStep <$> state.instructions.steps

                  , HH.button
                      [ HP.class_ (H.ClassName "btn btn-outline btn-block mt-4 border-dashed")
                      , HP.type_ ButtonButton
                      , HE.onClick (\_ -> NewStep)
                      ]
                      [ HH.text "+ Add Next Step" ]
                  ]
              ]
          , HH.aside [ HP.class_ (H.ClassName "space-y-6") ]
              [ --materialsSection
                HH.div [ HP.class_ (H.ClassName "alert alert-info shadow-sm") ]
                  [ Svg.svg
                      [ SP.class_ (H.ClassName "stroke-current w-6 h-6")
                      , SP.fill NoColor
                      , SP.viewBox 0.0 0.0 24.0 24.0
                      ]
                      [ Svg.path
                          [ SP.strokeLineCap LineCapRound
                          , SP.strokeLineJoin LineJoinRound
                          , SP.strokeWidth 2.0
                          , SP.d
                              [ SP.m SP.Abs 13.0 16.0
                              , SP.h SP.Rel (-1.0)
                              , SP.v SP.Rel (-4.0)
                              , SP.h SP.Rel (-1.0)
                              , SP.m SP.Rel 1.0 (-4.0)
                              , SP.h SP.Rel 0.01
                              , SP.m SP.Abs 21.0 12.0
                              , SP.a SP.Rel 9.0 9.0 0.0 SP.Arc1 SP.Sweep1 (-18.0) 0.0
                              , SP.a SP.Rel 9.0 9.0 0.0 SP.Arc0 SP.Sweep1 18.0 0.0
                              , SP.z
                              ]
                          ]
                      ]
                  , HH.span_ [ HH.text "Pack items in the order you'd use them during setup!" ]
                  ]
              ]
          ]
      ]
    _ -> HH.form [] []

renderStep :: forall slots m. MonadAff m => MonadEffect m => PackingStep -> HH.ComponentHTML Action slots m
renderStep step = HH.div [ HP.class_ (H.ClassName "step-item card bg-base-200 shadow-sm border border-base-300") ]
  [ HH.div [ HP.class_ (H.ClassName "card-body p-4 flex flex-row gap-4") ]
      [ HH.div [ HP.class_ (H.ClassName "avatar placeholder") ]
          [ HH.label [ HP.class_ (H.ClassName "bg-neutral text-neutral-content rounded-lg w-24 h-24 flex flex-col items-center justify-center cursor-pointer hover:bg-neutral-focus overflow-hidden") ]
              [ HH.span [ HP.class_ (H.ClassName "flex flex-col items-center justify-center w-24 h-24") ] [ renderImage ]
              , HH.input [ HP.required true, HP.type_ InputFile, HP.class_ (H.ClassName "hidden"), HP.accept (mediaType (MediaType "image/*")) ]
              ]
          ]
      , HH.div [ HP.class_ (H.ClassName "flex-1") ]
          [ HH.div [ HP.class_ (H.ClassName "flex justify-between mb-2") ]
              [ HH.span [ HP.class_ (H.ClassName "badge badge-ghost step-number") ]
                  [ HH.text $ "Step " <> (show step.stepOrdinal) ]
              ]
          , HH.textarea
              [ HP.class_ (H.ClassName "textarea textarea-bordered w-full h-24")
              , HP.required true
              , HP.placeholder "Describe the component placement..."
              , HE.onValueInput (UpdateStepDescription step)
              , HP.value step.description
              ]
          ]
      , HH.button
          [ HE.onClick (\_ -> RemoveStep step)
          , HP.class_ (H.ClassName "btn btn-ghost btn-sm text-error")
          , HP.type_ ButtonButton
          ]
          [ HH.text "✕" ]
      ]
  ]

renderImage :: forall slots m. MonadAff m => MonadEffect m => HH.ComponentHTML Action slots m
renderImage = HH.span [ HP.class_ (H.ClassName "text-[10px] uppercase font-bold") ] [ HH.text "Add Photo" ]

renderExpansion :: forall slots m. MonadAff m => MonadEffect m => { gameId :: GameId, title :: String } -> HH.ComponentHTML Action slots m
renderExpansion { gameId, title } = HH.label [ HP.class_ (H.ClassName "label cursor-pointer hover:bg-base-200 rounded px-2") ]
  [ HH.span [ HP.class_ (H.ClassName "label-text") ] [ HH.text title ]
  , HH.input
      [ HP.type_ InputCheckbox
      , HP.class_ (H.ClassName "checkbox checkbox-sm checkbox-primary")
      , HP.value (unwrap gameId)
      , HE.onChange (\_ -> ToggleExpansion gameId)
      ]
  ]