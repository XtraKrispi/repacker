module Component.Instructions where

import Prelude

import Bgg (bggThing)
import Component.ConfirmationButton as ConfirmationButton
import Component.Helpers (addToast, classList)
import Component.Helpers as Helpers
import DOM.HTML.Indexed.InputAcceptType (mediaType)
import Data.Array (catMaybes, filter, find, intercalate, length, mapWithIndex, null, sortWith)
import Data.Either (Either(..))
import Data.Foldable (maximum)
import Data.Map (empty)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing, maybe)
import Data.MediaType (MediaType(..))
import Data.Newtype (unwrap, wrap)
import Data.Set as Set
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Data.Tuple.Nested ((/\))
import Data.UUID (genUUID, toString)
import Database.Instructions (fetchImagesForInstructions, fetchSingleInstructions)
import Database.Instructions as Database
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen (AttrName(..), get, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..), InputType(..))
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SP
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(..))
import Halogen.Svg.Attributes.StrokeLineJoin (StrokeLineJoin(..))
import Halogen.Svg.Elements as Svg
import Network.RemoteData (RemoteData(..))
import Network.RemoteData as RemoteData
import Route (Route(..), navigate)
import Store as S
import Supabase (Client)
import Type.Prelude (Proxy(..))
import Types (BoardGame, GameId, Image(..), Images, Instructions, InstructionsKey, PackingStep, SessionInfo)
import Web.Event.Event (Event, preventDefault, target)
import Web.File.File (name, toBlob)
import Web.File.FileList (item)
import Web.File.FileReader.Aff as FRA
import Web.HTML.HTMLInputElement (files, fromEventTarget)

type Slots = (deleteModal :: forall query. H.Slot query ConfirmationButton.Output Int)

_deleteModel = Proxy :: Proxy "deleteModal"

type CoreData =
  ( client :: Client
  , gameId :: GameId
  , session :: SessionInfo
  , existingKey :: Maybe InstructionsKey
  )

type State =
  { game :: RemoteData String BoardGame
  , instructions :: RemoteData String Instructions
  , isPrivate :: Boolean
  , images :: Images
  | CoreData
  }

validate :: State -> Array (Tuple String String)
validate state =
  case state.instructions of
    Success instructions ->
      ( catMaybes
          [ if instructions.description == "" then
              Just ("description" /\ "A description is required")
            else Nothing
          , if null instructions.steps then
              Just ("steps" /\ "There must be at least one step to the packing instructions")
            else Nothing
          ]
      ) <> (instructions.steps >>= validateStep)
    _ -> []

validateStep :: PackingStep -> Array (Tuple String String)
validateStep { description, image, stepOrdinal } = catMaybes
  [ if description == "" then
      Just (("step" <> show stepOrdinal <> ":description") /\ ("Step " <> show stepOrdinal <> " requires a description"))
    else Nothing
  , if isNothing image then
      Just (("step" <> show stepOrdinal <> ":image") /\ ("Step " <> show stepOrdinal <> " requires an image"))
    else Nothing
  ]

isValid :: Array (Tuple String String) -> String -> Boolean
isValid validationErrors field = maybe true (const false) $ find (\(f /\ _) -> field == f) validationErrors

type Input = { | CoreData }

defaultInstructions :: Instructions
defaultInstructions =
  { description: ""
  , allowsSleeves: false
  , requiresBaggies: false
  , steps: []
  , includedExpansions: wrap Set.empty
  , otherMaterials: ""
  , customInsert: ""
  }

defaultStep :: Int -> PackingStep
defaultStep ordinal =
  { description: ""
  , image: Nothing
  , stepOrdinal: ordinal
  }

data Action
  = Initialize
  | UpdateInstructionsDescription String
  | NewStep
  | ToggleExpansion GameId
  | RemoveStep PackingStep
  | UpdateStepDescription PackingStep String
  | UpdateOtherMaterials String
  | ToggleSleeves
  | ToggleBaggies
  | UpdateCustomInsertLink String
  | ImageUploaded PackingStep Event
  | UpdatePrivacy Boolean
  | Save Event
  | Delete

component :: forall query output m. MonadEffect m => MonadAff m => MonadStore S.Action S.Store m => H.Component query Input output m
component = H.mkComponent
  { initialState
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , initialize = Just Initialize
      }
  , render
  }

initialState :: Input -> State
initialState { client, gameId, session, existingKey } =
  { client
  , gameId
  , session
  , game: NotAsked
  , instructions: NotAsked
  , existingKey
  , images: empty
  , isPrivate: false
  }

handleAction :: forall slots output m. MonadAff m => MonadEffect m => MonadStore S.Action S.Store m => Action -> H.HalogenM State Action slots output m Unit
handleAction Initialize = do
  { gameId, existingKey, client, session } <- get
  modify_ \state -> state
    { game = Loading
    }
  eThing <- liftAff $ bggThing gameId
  case existingKey of
    Just key -> do
      modify_ _ { instructions = Loading }
      mInstructions <- liftAff $ fetchSingleInstructions client (Just session.userId) key
      case mInstructions of
        Nothing -> do
          addToast { message: "Instructions couldn't be found, redirecting back to game page", severity: S.Error }
          navigate (GameR gameId)
        Just { createdBy } | createdBy /= session.userId -> navigate (ViewInstructionsR gameId key)
        Just { instructions } -> do
          images <- liftAff $ fetchImagesForInstructions client key
          modify_ _
            { images = images
            , instructions = Success instructions
            }
          pure unit
    Nothing -> modify_ _ { instructions = Success defaultInstructions }
  modify_ _ { game = RemoteData.fromEither eThing }
handleAction (UpdateInstructionsDescription str) = modify_ $ \state -> state { instructions = map _ { description = str } state.instructions }
handleAction NewStep = do
  state <- get
  case state.instructions of
    Success instructions -> do
      let nextOrdinal = 1 + fromMaybe 0 (maximum (_.stepOrdinal <$> instructions.steps))
      modify_ _ { instructions = Success (instructions { steps = instructions.steps <> [ defaultStep nextOrdinal ] }) }
    _ -> pure unit
handleAction (ToggleExpansion gameId) = do
  state <- get
  case state.instructions of
    Success instructions -> do
      let
        new =
          wrap $
            if Set.member gameId (unwrap instructions.includedExpansions) then
              Set.delete gameId (unwrap instructions.includedExpansions)
            else Set.insert gameId (unwrap instructions.includedExpansions)

      modify_ _ { instructions = Success (instructions { includedExpansions = new }) }
    _ -> pure unit
handleAction (RemoveStep step) = do
  state <- get
  case state.instructions of
    Success instructions -> do
      let
        newImages =
          case find (_ == step) instructions.steps >>= _.image of
            Just image -> Map.delete image state.images
            Nothing -> state.images
      let filtered = filter (\s -> s /= step) $ sortWith _.stepOrdinal instructions.steps
      let reordered = mapWithIndex (\i s -> s { stepOrdinal = i + 1 }) filtered
      modify_ _ { instructions = Success (instructions { steps = reordered }), images = newImages }
    _ -> pure unit
handleAction (UpdateStepDescription step str) = do
  state <- get
  case state.instructions of
    Success instructions ->
      modify_ (_ { instructions = Success (instructions { steps = map (\s -> if s == step then step { description = str } else s) instructions.steps }) })
    _ -> pure unit
handleAction (UpdateOtherMaterials str) = do
  state <- get
  case state.instructions of
    Success instructions ->
      modify_ (_ { instructions = Success (instructions { otherMaterials = str }) })
    _ -> pure unit

handleAction ToggleSleeves = do
  state <- get
  case state.instructions of
    Success instructions ->
      modify_ (_ { instructions = Success (instructions { allowsSleeves = not instructions.allowsSleeves }) })
    _ -> pure unit
handleAction ToggleBaggies = do
  state <- get
  case state.instructions of
    Success instructions ->
      modify_ (_ { instructions = Success (instructions { requiresBaggies = not instructions.requiresBaggies }) })
    _ -> pure unit
handleAction (UpdateCustomInsertLink str) = do
  state <- get
  case state.instructions of
    Success instructions ->
      modify_ (_ { instructions = Success (instructions { customInsert = str }) })
    _ -> pure unit
handleAction (ImageUploaded step evt) = do
  state <- get
  case state.instructions of
    Success instructions -> do
      let maybeInput = fromEventTarget =<< target evt
      case maybeInput of
        Just input -> do
          fs <- liftEffect $ files input
          case fs of
            Just files -> do
              case item 0 files of
                Just file -> do
                  let blob = toBlob file
                  imageKey <- liftEffect genUUID
                  let fileName = toString imageKey <> "__" <> name file
                  imageContent <- liftAff $ FRA.readAsDataURL blob
                  modify_ _
                    { instructions = Success
                        ( instructions
                            { steps = map
                                ( \s ->
                                    if s == step then
                                      s { image = Just fileName }
                                    else s
                                )
                                instructions.steps
                            }
                        )
                    , images = Map.insertWith (\_ n -> n) fileName (Uploaded file imageContent) state.images
                    }
                Nothing -> pure unit
            Nothing -> pure unit
        Nothing -> do
          pure unit
    _ -> pure unit
handleAction (UpdatePrivacy checked) = do
  modify_ _ { isPrivate = checked }
handleAction (Save evt) = do
  liftEffect $ preventDefault evt
  state <- get
  if null (validate state) then do
    case state.instructions of
      Success instructions -> do
        case state.existingKey of
          Just key -> do
            results <- liftAff $ Database.updateInstructions state.client state.gameId state.isPrivate key instructions state.images
            case results of
              Right _ -> addToast { message: "Instructions have been saved.", severity: S.Success }
              Left _ -> addToast
                { message: "There was a problem saving the instructions, please try again."
                , severity: S.Error
                }
          Nothing -> do

            newKey <- wrap <$> liftEffect genUUID
            results <- liftAff $ Database.newInstructions state.client state.gameId state.isPrivate newKey instructions state.images
            case results of
              Right _ -> do
                addToast
                  { message: "Instructions have been saved."
                  , severity: S.Success
                  }
                navigate $ UpdateInstructionsR state.gameId newKey
              Left _err -> do
                addToast
                  { message: "There was a problem saving the instructions, please try again."
                  , severity: S.Error
                  }

      _ -> pure unit
  else
    pure unit
handleAction Delete = do
  -- TODO: Plumb this through
  pure unit

render :: forall m. MonadAff m => MonadEffect m => State -> H.ComponentHTML Action Slots m
render state =
  case state.game /\ state.instructions of
    Success game /\ Success instructions ->
      let
        validationErrors = validate state
      in
        HH.div [ HP.class_ (H.ClassName "p-8") ]
          [ HH.div [ HP.class_ (H.ClassName "max-w-4xl mx-auto space-y-6") ]
              [ HH.header [ HP.class_ (H.ClassName "flex justify-between items-center") ]
                  [ HH.div []
                      [ HH.h1 [ HP.class_ (H.ClassName "text-3xl font-bold text-primary") ]
                          [ HH.text "Create Repack Guide" ]
                      , HH.p [ HP.class_ (H.ClassName "text-base-content/70") ] [ HH.text "Instructions for organizational perfection." ]
                      ]
                  , HH.div [ HP.class_ (H.ClassName "flex gap-4") ]
                      [ HH.label [ HP.class_ (H.ClassName "label") ]
                          [ HH.input
                              [ HP.type_ InputCheckbox
                              , HP.class_ (H.ClassName "toggle toggle-accent")
                              , HP.checked state.isPrivate
                              , HE.onChecked UpdatePrivacy
                              ]
                          , HH.span
                              [ HP.class_
                                  ( Helpers.classList
                                      [ "transition-all" /\ true
                                      , "text-accent" /\ state.isPrivate
                                      ]
                                  )
                              ]
                              [ HH.text "Private (visible only to me)" ]
                          ]
                      , HH.slot _deleteModel 0 ConfirmationButton.component { buttonText: "Delete Guide", buttonCss: H.ClassName "btn btn-error", modalContent: "Are you sure you want to delete these instructions? The operation cannot be undone." } (\_ -> Delete)
                      -- HH.button
                      -- [ HP.class_ (H.ClassName "btn btn-error")
                      -- , HP.type_ ButtonButton
                      -- ]
                      -- [ HH.text "Delete Guide"
                      -- ]
                      , HH.button
                          [ HP.class_ $ classList
                              [ "btn" /\ true
                              , "btn-primary" /\ true
                              , "px-8" /\ true
                              , "pointer-events-auto" /\ true
                              , "cursor-not-allowed" /\ not (null validationErrors)
                              ]
                          , HP.type_ ButtonSubmit
                          , HP.attr (AttrName "form") "instructions-form"
                          , HP.disabled (not $ null validationErrors)
                          , HP.title $ intercalate "\n" $ Tuple.snd <$> validationErrors
                          ]
                          [ HH.text "Publish Guide"
                          ]
                      ]
                  ]
              , instructionsForm validationErrors game instructions state.images
              ]
          ]
    _ -> HH.div [ HP.class_ (H.ClassName "p-8") ]
      [ HH.div [ HP.class_ (H.ClassName "max-w-4xl mx-auto flex justify-center items-center flex-col gap-4 py-16 text-base-content/70") ]
          [ HH.span [ HP.class_ (H.ClassName "loading loading-spinner loading-lg text-primary") ] []
          , HH.p [ HP.class_ (H.ClassName "text-lg") ] [ HH.text "Loading..." ]
          ]
      ]

instructionsForm :: forall slots m. MonadAff m => MonadEffect m => Array (Tuple String String) -> BoardGame -> Instructions -> Images -> H.ComponentHTML Action slots m
instructionsForm validationErrors game instructions images =
  HH.form [ HP.id "instructions-form", HE.onSubmit Save ]
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
                            [ HP.class_ $ classList
                                [ "input" /\ true
                                , "input-bordered" /\ true
                                , "w-full" /\ true
                                , "input-error" /\ (not $ isValid validationErrors "description")
                                ]
                            , HP.placeholder "e.g. Scythe Legendary Box Layout"
                            , HP.required true
                            , HE.onValueInput UpdateInstructionsDescription
                            , HP.value instructions.description
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
                , HH.div [ HP.class_ (H.ClassName "space-y-4") ] $ renderStep validationErrors images <$> instructions.steps

                , HH.button
                    [ HP.class_ (H.ClassName "btn btn-outline btn-block mt-4 border-dashed")
                    , HP.type_ ButtonButton
                    , HE.onClick (\_ -> NewStep)
                    ]
                    [ HH.text $ if length instructions.steps == 0 then "+ Add First Step" else "+ Add Next Step" ]
                ]
            ]
        , HH.aside [ HP.class_ (H.ClassName "space-y-6") ]
            [ materialsSection instructions
            , HH.div [ HP.class_ (H.ClassName "alert alert-info shadow-sm") ]
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

materialsSection :: forall slots m. MonadAff m => MonadEffect m => Instructions -> HH.ComponentHTML Action slots m
materialsSection instructions =
  HH.div [ HP.class_ (H.ClassName "card bg-base-200 shadow-xl") ]
    [ HH.div [ HP.class_ (H.ClassName "card-body") ]
        [ HH.h2 [ HP.class_ (H.ClassName "card-title text-secondary") ] [ HH.text "Materials Required" ]
        , HH.div [ HP.class_ (H.ClassName "form-control ") ]
            [ HH.label [ HP.class_ (H.ClassName "label cursor-pointer hover:bg-base-200 rounded px-2") ]
                [ HH.span [ HP.class_ (H.ClassName "label-text") ] [ HH.text "Supports Sleeves?" ]
                , HH.input
                    [ HP.type_ InputCheckbox
                    , HP.class_ (H.ClassName "checkbox checkbox-sm checkbox-primary")
                    , HE.onChange (\_ -> ToggleSleeves)
                    , HP.checked (instructions.allowsSleeves)
                    ]
                ]
            ]
        , HH.div [ HP.class_ (H.ClassName "form-control mb-4") ]
            [ HH.label [ HP.class_ (H.ClassName "label cursor-pointer hover:bg-base-200 rounded px-2") ]
                [ HH.span [ HP.class_ (H.ClassName "label-text") ] [ HH.text "Requires Baggies?" ]
                , HH.input
                    [ HP.type_ InputCheckbox
                    , HP.class_ (H.ClassName "checkbox checkbox-sm checkbox-primary")
                    , HE.onChange (\_ -> ToggleBaggies)
                    , HP.checked (instructions.requiresBaggies)
                    ]
                ]
            ]
        , HH.div [ HP.class_ (H.ClassName "form-control mb-4 flex flex-col gap-1") ]
            [ HH.label [ HP.class_ (H.ClassName "label") ]
                [ HH.span [ HP.class_ (H.ClassName "label-text font-bold") ]
                    [ HH.text "Custom Insert Link" ]
                ]
            , HH.input
                [ HP.type_ InputUrl
                , HP.class_ (H.ClassName "input input-sm input-bordered w-full")
                , HP.placeholder "Link (if applicable)"
                , HE.onValueInput UpdateCustomInsertLink
                , HP.value (instructions.customInsert)
                ]
            ]
        , HH.div [ HP.class_ (H.ClassName "form-control mb-4 flex flex-col gap-1") ]
            [ HH.label [ HP.class_ (H.ClassName "label") ]
                [ HH.span [ HP.class_ (H.ClassName "label-text font-bold") ]
                    [ HH.text "Other Materials" ]
                ]

            , HH.textarea
                [ HP.class_ (H.ClassName "textarea textarea-bordered w-full h-24")
                , HP.placeholder "Any additional items or notes..."
                , HE.onValueInput UpdateOtherMaterials
                , HP.value instructions.otherMaterials
                ]
            ]
        ]

    ]

renderStep :: forall slots m. MonadAff m => MonadEffect m => Array (Tuple String String) -> Images -> PackingStep -> HH.ComponentHTML Action slots m
renderStep validationErrors images step =
  HH.div [ HP.class_ (H.ClassName "step-item card bg-base-200 shadow-sm border border-base-300") ]
    [ HH.div [ HP.class_ (H.ClassName "card-body p-4 flex flex-row gap-4") ]
        [ HH.div [ HP.class_ (H.ClassName "avatar placeholder") ]
            [ HH.label
                [ HP.class_ $ classList
                    [ "bg-neutral" /\ true
                    , "text-neutral-content" /\ true
                    , "rounded-lg" /\ true
                    , "w-24" /\ true
                    , "h-24" /\ true
                    , "flex" /\ true
                    , "flex-col" /\ true
                    , "items-center" /\ true
                    , "justify-center" /\ true
                    , "cursor-pointer" /\ true
                    , "hover:bg-neutral-focus" /\ true
                    , "overflow-hidden" /\ true
                    , "border-error" /\ (not $ isValid validationErrors ("step" <> show step.stepOrdinal <> ":image"))
                    , "border-1" /\ (not $ isValid validationErrors ("step" <> show step.stepOrdinal <> ":image"))
                    ]
                ]
                [ HH.span [ HP.class_ (H.ClassName "flex flex-col items-center justify-center w-24 h-24") ] [ renderImage (step.image >>= (\image -> Map.lookup image images)) ]
                , HH.input
                    [ HP.required true
                    , HP.type_ InputFile
                    , HP.class_ (H.ClassName "hidden")
                    , HP.accept (mediaType (MediaType "image/*"))
                    , HE.onChange (ImageUploaded step)
                    ]
                ]
            ]
        , HH.div [ HP.class_ (H.ClassName "flex-1") ]
            [ HH.div [ HP.class_ (H.ClassName "flex justify-between mb-2") ]
                [ HH.span [ HP.class_ (H.ClassName "badge badge-ghost step-number") ]
                    [ HH.text $ "Step " <> (show step.stepOrdinal) ]
                ]
            , HH.textarea
                [ HP.class_ $ classList
                    [ "textarea" /\ true
                    , "textarea-bordered" /\ true
                    , "w-full" /\ true
                    , "h-24" /\ true
                    , "input-error" /\ (not $ isValid validationErrors $ "step" <> show step.stepOrdinal <> ":description")
                    ]
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

renderImage :: forall slots m. MonadAff m => MonadEffect m => Maybe Image -> HH.ComponentHTML Action slots m
renderImage Nothing =
  HH.span [ HP.class_ (H.ClassName "text-[10px] uppercase font-bold") ]
    [ HH.text "Add Photo" ]
renderImage (Just (Uploaded _ imageContent)) = HH.img [ HP.src imageContent ]
renderImage (Just (Downloaded imageContent)) = HH.img [ HP.src imageContent ]

renderExpansion :: forall slots m. MonadAff m => MonadEffect m => { gameId :: GameId, title :: String } -> HH.ComponentHTML Action slots m
renderExpansion { gameId, title } =
  HH.label [ HP.class_ (H.ClassName "label cursor-pointer hover:bg-base-200 rounded px-2") ]
    [ HH.span [ HP.class_ (H.ClassName "label-text") ] [ HH.text title ]
    , HH.input
        [ HP.type_ InputCheckbox
        , HP.class_ (H.ClassName "checkbox checkbox-sm checkbox-primary")
        , HP.value (unwrap gameId)
        , HE.onChange (\_ -> ToggleExpansion gameId)
        ]
    ]