module Component.ViewInstructions where

import Prelude

import Bgg (bggThing)
import Component.ConfirmationButton as ConfirmationButton
import Component.Helpers (addToast)
import Data.Array (catMaybes, findMap, fromFoldable, index, length, mapWithIndex, null)
import Data.Either (Either(..), either)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Database.Instructions (deleteInstructions, fetchImagesForInstructions, fetchSingleInstructions)
import Database.Profile (fetchProfile)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Halogen (get, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events (onClick)
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP
import Halogen.Store.Monad (class MonadStore)
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SP
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(..))
import Halogen.Svg.Attributes.StrokeLineJoin (StrokeLineJoin(..))
import Halogen.Svg.Elements as Svg
import Network.RemoteData (RemoteData(..), withDefault)
import Network.RemoteData as RemoteData
import Route (Route(..), navigate, routeCodec)
import Routing.Duplex (print)
import Store as S
import Supabase (Client, UserId)
import Type.Proxy (Proxy(..))
import Types (BoardGame, FileContents, GameId, Image(..), Images, IncludedExpansions(..), Instructions, InstructionsKey, PackingStep, Profile, SessionInfo, InstructionsWithUser)

type Slots = (deleteModal :: forall query. H.Slot query ConfirmationButton.Output Int)

_deleteModel = Proxy :: Proxy "deleteModal"

type CoreData =
  ( client :: Client
  , gameId :: GameId
  , instructionsKey :: InstructionsKey
  , session :: Maybe SessionInfo
  )

type Input =
  { | CoreData
  }

data ViewMode = ListView | CarouselView

derive instance eqViewMode :: Eq ViewMode

type State =
  { game :: RemoteData String BoardGame
  , instructions :: RemoteData String InstructionsWithUser
  , authorProfile :: RemoteData String (Maybe Profile)
  , images :: Images
  , viewMode :: ViewMode
  , carouselIndex :: Int
  , expandedImage :: Maybe FileContents
  | CoreData
  }

data Action
  = Initialize
  | SetViewMode ViewMode
  | NextSlide
  | PrevSlide
  | GoToSlide Int
  | ExpandImage FileContents
  | CloseExpandedImage
  | Delete
  | Upvote
  | Downvote

component :: forall query output m. MonadEffect m => MonadAff m => MonadStore S.Action S.Store m => H.Component query Input output m
component = H.mkComponent
  { initialState
  , eval: H.mkEval H.defaultEval
      { initialize = Just Initialize
      , handleAction = handleAction
      }
  , render
  }

initialState :: Input -> State
initialState { client, gameId, instructionsKey, session } =
  { client
  , gameId
  , instructionsKey
  , session
  , instructions: NotAsked
  , game: NotAsked
  , authorProfile: NotAsked
  , viewMode: ListView
  , carouselIndex: 0
  , expandedImage: Nothing
  , images: Map.empty
  }

handleAction :: forall output m. MonadEffect m => MonadAff m => MonadStore S.Action S.Store m => Action -> H.HalogenM State Action Slots output m Unit
handleAction Initialize = do
  { client, instructionsKey, gameId, session } <- get
  modify_ _
    { instructions = Loading
    , game = Loading
    , authorProfile = Loading
    }
  instructions <- liftAff $ fetchSingleInstructions client (_.userId <$> session) instructionsKey
  game <- liftAff $ bggThing gameId
  images <- liftAff $ fetchImagesForInstructions client instructionsKey
  profile <- liftAff $ traverse (\{ createdBy } -> fetchProfile client createdBy) instructions
  modify_ _
    { instructions = RemoteData.fromMaybe instructions
    , game = RemoteData.fromEither game
    , images = images
    , authorProfile = RemoteData.fromEither $ fromMaybe (Left "Invalid instructions") profile
    }
handleAction (SetViewMode mode) = modify_ _ { viewMode = mode, carouselIndex = 0 }
handleAction NextSlide = modify_ \state ->
  let
    total = length $ withDefault [] $ map (_.instructions >>> _.steps) state.instructions
  in
    state { carouselIndex = if state.carouselIndex + 1 >= total then 0 else state.carouselIndex + 1 }
handleAction PrevSlide = modify_ \state ->
  let
    total = length $ withDefault [] $ map (_.instructions >>> _.steps) state.instructions
  in
    state { carouselIndex = if state.carouselIndex <= 0 then total - 1 else state.carouselIndex - 1 }
handleAction (GoToSlide i) = modify_ _ { carouselIndex = i }
handleAction (ExpandImage img) = modify_ _ { expandedImage = Just img }
handleAction CloseExpandedImage = modify_ _ { expandedImage = Nothing }
handleAction Delete = do
  { client, gameId, instructionsKey } <- get
  results <- liftAff $ deleteInstructions client instructionsKey
  either
    (const $ addToast { message: "There was a problem deleting the instructions, please try again.", severity: S.Error })
    (const $ navigate (GameR gameId))
    results
handleAction Upvote = pure unit
handleAction Downvote = pure unit

render :: forall m. MonadAff m => MonadEffect m => State -> H.ComponentHTML Action Slots m
render state = case state.game /\ state.instructions /\ state.authorProfile of
  (Success game /\ Success { createdBy, instructions } /\ Success author) -> HH.div [ HP.class_ (H.ClassName "flex flex-col gap-6 max-w-4xl mx-auto") ]
    [ renderMetadata createdBy author state.gameId game state.instructionsKey instructions (_.userId <$> state.session)
    , HH.div [ HP.class_ (H.ClassName "divider") ] []
    , renderStepsHeader state.viewMode (not $ null instructions.steps)
    , case state.viewMode of
        ListView -> renderStepsList state.images instructions.steps
        CarouselView -> renderCarousel state.images instructions.steps state.carouselIndex
    , renderImageLightbox state.expandedImage
    ]
  _ ->
    let
      errors = catMaybes [ failureError state.game, failureError state.instructions, failureError state.authorProfile ]
    in
      if null errors then renderLoading else renderError

-- | Pull the error message out of a failed request, ignoring every other state.
failureError :: forall a. RemoteData String a -> Maybe String
failureError (Failure e) = Just e
failureError _ = Nothing

-- | Shown while any of the page's requests are still in flight.
renderLoading :: forall slots m. MonadAff m => MonadEffect m => H.ComponentHTML Action slots m
renderLoading = HH.div [ HP.class_ (H.ClassName "flex flex-col items-center justify-center gap-4 py-24 max-w-4xl mx-auto") ]
  [ HH.span [ HP.class_ (H.ClassName "loading loading-spinner loading-lg text-primary") ] []
  , HH.p [ HP.class_ (H.ClassName "text-base-content/60") ] [ HH.text "Loading packing guide…" ]
  ]

-- | Shown when one or more requests fail. Deliberately generic so we don't leak
-- | any internal details to the user; the retry button re-runs every request.
renderError :: forall slots m. MonadAff m => MonadEffect m => H.ComponentHTML Action slots m
renderError = HH.div [ HP.class_ (H.ClassName "max-w-4xl mx-auto py-16") ]
  [ HH.div [ HP.class_ (H.ClassName "alert alert-error flex-col items-start gap-3") ]
      [ HH.div [ HP.class_ (H.ClassName "flex items-center gap-2") ]
          [ errorIcon
          , HH.h2 [ HP.class_ (H.ClassName "font-bold") ] [ HH.text "Something went wrong" ]
          ]
      , HH.p [ HP.class_ (H.ClassName "text-sm") ]
          [ HH.text "We couldn't load this packing guide. Please try again." ]
      , HH.button
          [ HP.class_ (H.ClassName "btn btn-sm")
          , HP.type_ ButtonButton
          , HE.onClick (\_ -> Initialize)
          ]
          [ HH.text "Try again" ]
      ]
  ]

-- | A warning triangle to head up the error state.
errorIcon :: forall slots m. MonadAff m => MonadEffect m => H.ComponentHTML Action slots m
errorIcon = Svg.svg
  [ SP.class_ (H.ClassName "h-6 w-6 shrink-0")
  , SP.fill NoColor
  , SP.viewBox 0.0 0.0 24.0 24.0
  , SP.stroke (Named "currentColor")
  ]
  [ Svg.path
      [ SP.strokeLineCap LineCapRound
      , SP.strokeLineJoin LineJoinRound
      , SP.strokeWidth 2.0
      , SP.d
          [ SP.m SP.Abs 12.0 9.0
          , SP.v SP.Rel 3.75
          , SP.m SP.Abs 12.0 15.75
          , SP.h SP.Rel 0.0
          , SP.m SP.Abs 10.29 3.86
          , SP.l SP.Abs 1.82 18.0
          , SP.l SP.Abs 19.71 18.0
          , SP.z
          ]
      ]
  ]

-- | A full-screen overlay showing the clicked image at its natural size (up to
-- | the viewport bounds). Clicking anywhere dismisses it.
renderImageLightbox :: forall slots m. MonadAff m => MonadEffect m => Maybe FileContents -> H.ComponentHTML Action slots m
renderImageLightbox Nothing = HH.text ""
renderImageLightbox (Just img) = HH.div
  [ HP.class_ (H.ClassName "fixed inset-0 z-50 flex items-center justify-center bg-black/80 p-4 cursor-zoom-out")
  , HE.onClick (\_ -> CloseExpandedImage)
  ]
  [ HH.img
      [ HP.class_ (H.ClassName "max-w-full max-h-full object-contain")
      , HP.src img
      , HP.alt "Expanded step image"
      ]
  , HH.button
      [ HP.class_ (H.ClassName "btn btn-circle btn-sm absolute top-4 right-4")
      , HP.type_ ButtonButton
      , HP.title "Close"
      , HE.onClick (\_ -> CloseExpandedImage)
      ]
      [ HH.text "✕" ]
  ]

renderMetadata :: forall m. MonadAff m => MonadEffect m => UserId -> Maybe Profile -> GameId -> BoardGame -> InstructionsKey -> Instructions -> Maybe UserId -> H.ComponentHTML Action Slots m
renderMetadata createdBy author gameId game instructionsKey instructions mSessionUser = HH.div [ HP.class_ (H.ClassName "card bg-base-200 shadow-xl") ]
  [ HH.div [ HP.class_ (H.ClassName "card-body") ]
      [ HH.div [ HP.class_ (H.ClassName "flex justify-between items-start gap-4") ]
          [ HH.div [ HP.class_ (H.ClassName "flex flex-col gap-1") ]
              [ HH.div [ HP.class_ (H.ClassName "flex gap-4 items-center") ]
                  [ HH.a
                      [ HP.href ("https://boardgamegeek.com/boardgame/" <> unwrap game.bggId)
                      , HP.target "_blank"
                      , HP.class_ (H.ClassName "flex items-center gap-2 text-3xl font-bold text-primary hover:underline hover:underline-offset-8")
                      ]
                      [ HH.span []
                          [ HH.text game.title
                          , maybe (HH.text "") (\y -> HH.text (" (" <> show y <> ")")) game.yearPublished
                          ]
                      , Svg.svg
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
                                  [ SP.m SP.Abs 13.5 6.0
                                  , SP.h SP.Abs 5.25
                                  , SP.a SP.Abs 2.25 2.25 0.0 SP.Arc0 SP.Sweep0 3.0 8.25
                                  , SP.v SP.Rel 10.5
                                  , SP.a SP.Abs 2.25 2.25 0.0 SP.Arc0 SP.Sweep0 5.25 21.0
                                  , SP.h SP.Rel 10.5
                                  , SP.a SP.Abs 2.25 2.25 0.0 SP.Arc0 SP.Sweep0 18.0 18.75
                                  , SP.v SP.Abs 10.5
                                  , SP.m SP.Rel (-10.5) 6.0
                                  , SP.l SP.Abs 21.0 3.0
                                  , SP.m SP.Rel 0.0 0.0
                                  , SP.h SP.Rel (-5.25)
                                  , SP.m SP.Abs 21.0 3.0
                                  , SP.v SP.Rel 5.25
                                  ]
                              ]
                          ]
                      ]
                  , HH.div [ HP.class_ (H.ClassName "flex gap-2") ]
                      [ HH.i
                          [ HP.class_ (H.ClassName "cursor-pointer transition-all hover:text-secondary")
                          , onClick (const Downvote)
                          ]
                          [ downvoteIcon ]
                      , HH.i
                          [ HP.class_ (H.ClassName "cursor-pointer transition-all hover:text-secondary")
                          , onClick (const Upvote)
                          ]
                          [ upvoteIcon ]
                      ]
                  ]
              , HH.p [ HP.class_ (H.ClassName "text-sm text-base-content/60") ]
                  [ HH.text $ "Packing guide by " <> extractAuthorName author ]
              ]
          , HH.div [ HP.class_ (H.ClassName "flex gap-2") ]
              [ editLink
              , deleteLink
              ]
          ]
      , HH.p [ HP.class_ (H.ClassName "text-base-content/80 mt-2") ]
          [ HH.text instructions.description ]
      , HH.div [ HP.class_ (H.ClassName "flex flex-wrap gap-2 mt-4") ] $
          [ HH.span [ HP.class_ (H.ClassName "badge badge-ghost") ]
              [ HH.text $ show (length instructions.steps) <> " step" <> if length instructions.steps == 1 then "" else "s" ]
          ]
            <>
              ( if instructions.allowsSleeves then
                  [ HH.span [ HP.class_ (H.ClassName "badge badge-info") ] [ HH.text "Sleeves" ] ]
                else []
              )
            <>
              ( if instructions.requiresBaggies then
                  [ HH.span [ HP.class_ (H.ClassName "badge badge-info") ] [ HH.text "Baggies" ] ]
                else []
              )
            <>
              ( if instructions.customInsert /= "" then
                  [ HH.span [ HP.class_ (H.ClassName "badge badge-secondary") ] [ HH.text "Custom insert" ] ]
                else []
              )
      , HH.div [ HP.class_ (H.ClassName "grid grid-cols-1 md:grid-cols-2 gap-4 mt-4") ]
          [ renderExpansions game.expansions instructions.includedExpansions
          , renderMaterials instructions
          ]
      ]
  ]
  where
  editLink
    | mSessionUser == Just createdBy = HH.a
        [ HP.class_ (H.ClassName "btn btn-sm btn-secondary")
        , HP.href ("#" <> print routeCodec (UpdateInstructionsR gameId instructionsKey))
        ]
        [ HH.text "Edit" ]
    | otherwise = HH.text ""
  deleteLink
    | mSessionUser == Just createdBy = HH.slot _deleteModel 0 ConfirmationButton.component { buttonText: "Delete", buttonCss: H.ClassName "btn btn-error btn-sm", modalContent: "Are you sure you want to delete these instructions? The operation cannot be undone." } (const Delete)
    | otherwise = HH.text ""

extractAuthorName :: Maybe Profile -> String
extractAuthorName Nothing = "unknown"
extractAuthorName (Just profile)
  | profile.username /= "" = profile.username
  | otherwise = unwrap profile.email

renderExpansions
  :: forall slots m
   . MonadAff m
  => MonadEffect m
  => Array
       { title :: String
       , gameId :: GameId
       }
  -> IncludedExpansions
  -> H.ComponentHTML Action slots m
renderExpansions gameExpansions (IncludedExpansions includedExpansions) =
  let
    expansions = catMaybes $ (\exp -> findMap (\{ gameId, title } -> if gameId == exp then Just title else Nothing) gameExpansions) <$> (fromFoldable includedExpansions)
  in
    HH.div []
      [ HH.h3 [ HP.class_ (H.ClassName "font-bold text-secondary mb-2") ] [ HH.text "Included Expansions" ]
      , if null expansions then
          HH.p [ HP.class_ (H.ClassName "text-sm text-base-content/60") ] [ HH.text "Base game only" ]
        else
          HH.ul [ HP.class_ (H.ClassName "flex flex-col gap-1") ] $
            ( \title -> HH.li [ HP.class_ (H.ClassName "text-sm flex items-center gap-2") ]
                [ HH.span [ HP.class_ (H.ClassName "badge badge-xs badge-primary") ] []
                , HH.text title
                ]
            ) <$> expansions
      ]

renderMaterials :: forall slots m. MonadAff m => MonadEffect m => Instructions -> H.ComponentHTML Action slots m
renderMaterials instructions = HH.div []
  [ HH.h3 [ HP.class_ (H.ClassName "font-bold text-secondary mb-2") ] [ HH.text "Materials" ]
  , HH.div [ HP.class_ (H.ClassName "flex flex-col gap-2 text-sm") ]
      [ if instructions.customInsert == "" && instructions.otherMaterials == "" then
          HH.text "No additional materials required"
        else HH.text ""
      , if instructions.customInsert /= "" then
          HH.div []
            [ HH.span [ HP.class_ (H.ClassName "font-semibold") ] [ HH.text "Custom insert: " ]
            , HH.a
                [ HP.class_ (H.ClassName "link link-primary break-all")
                , HP.href instructions.customInsert
                , HP.target "_blank"
                ]
                [ HH.text instructions.customInsert ]
            ]
        else HH.text ""
      , if instructions.otherMaterials == "" then HH.text ""
        else HH.div []
          [ HH.span [ HP.class_ (H.ClassName "font-semibold") ] [ HH.text "Other materials: " ]
          , HH.span [ HP.class_ (H.ClassName "text-base-content/80") ] [ HH.text instructions.otherMaterials ]
          ]
      ]
  ]

renderStepsHeader :: forall slots m. MonadAff m => MonadEffect m => ViewMode -> Boolean -> H.ComponentHTML Action slots m
renderStepsHeader viewMode hasSteps = HH.div [ HP.class_ (H.ClassName "flex justify-between items-center") ]
  [ HH.h2 [ HP.class_ (H.ClassName "text-2xl font-bold text-primary") ] [ HH.text "Packing Steps" ]
  , if hasSteps then
      HH.div [ HP.class_ (H.ClassName "join") ]
        [ HH.button
            [ HP.class_ (H.ClassName $ "btn btn-sm btn-square join-item" <> if viewMode == ListView then " btn-active btn-primary" else "")
            , HP.type_ ButtonButton
            , HP.title "List view"
            , HE.onClick (\_ -> SetViewMode ListView)
            ]
            [ listIcon ]
        , HH.button
            [ HP.class_ (H.ClassName $ "btn btn-sm btn-square join-item" <> if viewMode == CarouselView then " btn-active btn-primary" else "")
            , HP.type_ ButtonButton
            , HP.title "Carousel view"
            , HE.onClick (\_ -> SetViewMode CarouselView)
            ]
            [ carouselIcon ]
        ]
    else HH.text ""
  ]

-- | A stacked-rows icon for the list view toggle.
listIcon :: forall slots m. MonadAff m => MonadEffect m => H.ComponentHTML Action slots m
listIcon = Svg.svg
  [ SP.class_ (H.ClassName "h-5 w-5")
  , SP.fill NoColor
  , SP.viewBox 0.0 0.0 24.0 24.0
  , SP.stroke (Named "currentColor")
  ]
  [ Svg.path
      [ SP.strokeLineCap LineCapRound
      , SP.strokeLineJoin LineJoinRound
      , SP.strokeWidth 2.0
      , SP.d
          [ SP.m SP.Abs 3.75 6.75
          , SP.h SP.Rel 16.5
          , SP.m SP.Abs 3.75 12.0
          , SP.h SP.Rel 16.5
          , SP.m SP.Abs 3.75 17.25
          , SP.h SP.Rel 16.5
          ]
      ]
  ]

-- | A center slide flanked by chevrons for the carousel view toggle.
carouselIcon :: forall slots m. MonadAff m => MonadEffect m => H.ComponentHTML Action slots m
carouselIcon = Svg.svg
  [ SP.class_ (H.ClassName "h-5 w-5")
  , SP.fill NoColor
  , SP.viewBox 0.0 0.0 24.0 24.0
  , SP.stroke (Named "currentColor")
  ]
  [ Svg.path
      [ SP.strokeLineCap LineCapRound
      , SP.strokeLineJoin LineJoinRound
      , SP.strokeWidth 2.0
      , SP.d
          [ SP.m SP.Abs 8.0 6.0
          , SP.h SP.Rel 8.0
          , SP.v SP.Rel 12.0
          , SP.h SP.Rel (-8.0)
          , SP.z
          , SP.m SP.Abs 5.0 9.0
          , SP.l SP.Abs 3.0 12.0
          , SP.l SP.Abs 5.0 15.0
          , SP.m SP.Abs 19.0 9.0
          , SP.l SP.Abs 21.0 12.0
          , SP.l SP.Abs 19.0 15.0
          ]
      ]
  ]

-- | A four-corner arrows icon hinting that the image can be expanded.
expandIcon :: forall slots m. MonadAff m => MonadEffect m => H.ComponentHTML Action slots m
expandIcon = Svg.svg
  [ SP.class_ (H.ClassName "h-10 w-10 text-white drop-shadow")
  , SP.fill NoColor
  , SP.viewBox 0.0 0.0 24.0 24.0
  , SP.stroke (Named "currentColor")
  ]
  [ Svg.path
      [ SP.strokeLineCap LineCapRound
      , SP.strokeLineJoin LineJoinRound
      , SP.strokeWidth 2.0
      , SP.d
          [ SP.m SP.Abs 3.75 8.25
          , SP.v SP.Rel (-4.5)
          , SP.h SP.Rel 4.5
          , SP.m SP.Abs 15.75 3.75
          , SP.h SP.Rel 4.5
          , SP.v SP.Rel 4.5
          , SP.m SP.Abs 20.25 15.75
          , SP.v SP.Rel 4.5
          , SP.h SP.Rel (-4.5)
          , SP.m SP.Abs 8.25 20.25
          , SP.h SP.Rel (-4.5)
          , SP.v SP.Rel (-4.5)
          ]
      ]
  ]

renderStepsList :: forall slots m. MonadAff m => MonadEffect m => Images -> Array PackingStep -> H.ComponentHTML Action slots m
renderStepsList _ [] = HH.div [ HP.class_ (H.ClassName "flex justify-center items-center py-16 text-base-content/50") ]
  [ HH.p [ HP.class_ (H.ClassName "text-lg") ] [ HH.text "No steps in this guide yet" ] ]
renderStepsList images steps = HH.div [ HP.class_ (H.ClassName "flex flex-col gap-4") ] $ renderStepCard images <$> steps

renderStepCard :: forall slots m. MonadAff m => MonadEffect m => Images -> PackingStep -> H.ComponentHTML Action slots m
renderStepCard images step = HH.div [ HP.class_ (H.ClassName "card md:card-side bg-base-200 shadow-sm border border-base-300") ]
  [ HH.figure [ HP.class_ (H.ClassName "md:w-64 shrink-0") ]
      [ case step.image >>= \k -> Map.lookup k images of
          Just img -> HH.div
            [ HP.class_ (H.ClassName "group relative w-full h-48 md:h-full cursor-zoom-in")
            , HP.title "Click to expand"
            , HE.onClick (\_ -> ExpandImage (getFileContents img))
            ]
            [ HH.img
                [ HP.class_ (H.ClassName "object-cover w-full h-full")
                , HP.src $ getFileContents img
                , HP.alt $ "Step " <> show step.stepOrdinal
                ]
            , HH.div
                [ HP.class_ (H.ClassName "absolute inset-0 flex items-center justify-center bg-black/40 opacity-0 transition-opacity group-hover:opacity-100") ]
                [ expandIcon ]
            ]
          Nothing -> HH.text ""
      ]
  , HH.div [ HP.class_ (H.ClassName "card-body") ]
      [ HH.span [ HP.class_ (H.ClassName "badge badge-ghost") ]
          [ HH.text $ "Step " <> show step.stepOrdinal ]
      , HH.p [ HP.class_ (H.ClassName "text-base-content/80") ] [ HH.text step.description ]
      ]
  ]

renderCarousel :: forall slots m. MonadAff m => MonadEffect m => Images -> Array PackingStep -> Int -> H.ComponentHTML Action slots m
renderCarousel _ [] _ = HH.div [ HP.class_ (H.ClassName "flex justify-center items-center py-16 text-base-content/50") ]
  [ HH.p [ HP.class_ (H.ClassName "text-lg") ] [ HH.text "No steps in this guide yet" ] ]
renderCarousel images steps currentIndex =
  let
    total = length steps
    safeIndex = if currentIndex >= total then 0 else currentIndex
    current = fromMaybe
      { stepOrdinal: 0
      , description: ""
      , image: Nothing
      }
      (index steps safeIndex)
    currentImage = current.image >>= \filename -> Map.lookup filename images
  in
    HH.div [ HP.class_ (H.ClassName "flex flex-col gap-4") ]
      [ HH.div [ HP.class_ (H.ClassName "card bg-base-200 shadow-xl overflow-hidden") ]
          [ HH.div [ HP.class_ (H.ClassName "relative bg-base-300") ]
              [ case currentImage of
                  Just img -> HH.div
                    [ HP.class_ (H.ClassName "group relative w-full h-96 cursor-zoom-in")
                    , HP.title "Click to expand"
                    , HE.onClick (\_ -> ExpandImage (getFileContents img))
                    ]
                    [ HH.img
                        [ HP.class_ (H.ClassName "object-contain w-full h-full")
                        , HP.src $ getFileContents img
                        , HP.alt $ "Step " <> show current.stepOrdinal
                        ]
                    , HH.div
                        [ HP.class_ (H.ClassName "absolute inset-0 flex items-center justify-center bg-black/40 opacity-0 transition-opacity group-hover:opacity-100") ]
                        [ expandIcon ]
                    ]
                  Nothing -> HH.text ""
              , HH.button
                  [ HP.class_ (H.ClassName "btn btn-circle absolute left-2 top-1/2 -translate-y-1/2")
                  , HP.type_ ButtonButton
                  , HE.onClick (\_ -> PrevSlide)
                  ]
                  [ HH.text "❮" ]
              , HH.button
                  [ HP.class_ (H.ClassName "btn btn-circle absolute right-2 top-1/2 -translate-y-1/2")
                  , HP.type_ ButtonButton
                  , HE.onClick (\_ -> NextSlide)
                  ]
                  [ HH.text "❯" ]
              , HH.span [ HP.class_ (H.ClassName "badge badge-neutral absolute top-2 right-2") ]
                  [ HH.text $ show (safeIndex + 1) <> " / " <> show total ]
              ]
          , HH.div [ HP.class_ (H.ClassName "card-body") ]
              [ HH.span [ HP.class_ (H.ClassName "badge badge-ghost") ]
                  [ HH.text $ "Step " <> show current.stepOrdinal ]
              , HH.p [ HP.class_ (H.ClassName "text-base-content/80") ] [ HH.text current.description ]
              ]
          ]
      , HH.div [ HP.class_ (H.ClassName "flex justify-center gap-2") ] $
          mapWithIndex
            ( \i _ -> HH.button
                [ HP.class_ (H.ClassName $ "btn btn-xs btn-circle" <> if i == safeIndex then " btn-primary" else " btn-ghost")
                , HP.type_ ButtonButton
                , HE.onClick (\_ -> GoToSlide i)
                ]
                [ HH.text $ show (i + 1) ]
            )
            steps
      ]

getFileContents :: Image -> FileContents
getFileContents (Downloaded f) = f
getFileContents (Uploaded _ f) = f

downvoteIcon ∷ forall a b. HH.HTML a b
downvoteIcon = Svg.svg
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
          [ SP.m SP.Abs 7.498 15.25
          , SP.h SP.Abs 4.372
          , SP.c SP.Rel (-1.026) 0.0 (-1.945) (-0.694) (-2.054) (-1.715)
          , SP.a SP.Rel 12.137 12.137 0.0 SP.Arc0 SP.Sweep1 (-0.068) (-1.285)
          , SP.c SP.Rel 0.0 (-2.848) 0.992 (-5.464) 2.649 (-7.521)
          , SP.c SP.Abs 5.287 4.247 5.886 4.0 6.504 4.0
          , SP.h SP.Rel 4.016
          , SP.a SP.Rel 4.5 4.5 0.0 SP.Arc0 SP.Sweep1 1.423 0.23
          , SP.l SP.Rel 3.114 1.04
          , SP.a SP.Rel 4.5 4.5 0.0 SP.Arc0 SP.Sweep0 1.423 0.23
          , SP.h SP.Rel 1.294
          , SP.m SP.Abs 7.498 15.25
          , SP.c SP.Rel 0.618 0.0 0.991 0.724 0.725 1.282
          , SP.a SP.Abs 7.471 7.471 0.0 SP.Arc0 SP.Sweep0 7.5 19.75
          , SP.a SP.Abs 2.25 2.25 0.0 SP.Arc0 SP.Sweep0 9.75 22.0
          , SP.a SP.Rel 0.75 0.75 0.0 SP.Arc0 SP.Sweep0 0.75 (-0.75)
          , SP.v SP.Rel (-0.633)
          , SP.c SP.Rel 0.0 (-0.573) 0.11 (-1.14) 0.322 (-1.672)
          , SP.c SP.Rel 0.304 (-0.76) 0.93 (-1.33) 1.653 (-1.715)
          , SP.a SP.Rel 9.04 9.04 0.0 SP.Arc0 SP.Sweep0 2.86 (-2.4)
          , SP.c SP.Rel 0.498 (-0.634) 1.226 (-1.08) 2.032 (-1.08)
          , SP.h SP.Rel 0.384
          , SP.m SP.Rel (-10.253) 1.5
          , SP.h SP.Abs 9.7
          , SP.m SP.Rel 8.075 (-9.75)
          , SP.c SP.Rel 0.01 0.05 0.027 0.1 0.05 0.148
          , SP.c SP.Rel 0.593 1.2 0.925 2.55 0.925 3.977
          , SP.c SP.Rel 0.0 1.487 (-0.36) 2.89 (-0.999) 4.125
          , SP.m SP.Rel 0.023 (-8.25)
          , SP.c SP.Rel (-0.076) (-0.365) 0.183 (-0.75) 0.575 (-0.75)
          , SP.h SP.Rel 0.908
          , SP.c SP.Rel 0.889 0.0 1.713 0.518 1.972 1.368
          , SP.c SP.Rel 0.339 1.11 0.521 2.287 0.521 3.507
          , SP.c SP.Rel 0.0 1.553 (-0.295) 3.036 (-0.831) 4.398
          , SP.c SP.Rel (-0.306) 0.774 (-1.086) 1.227 (-1.918) 1.227
          , SP.h SP.Rel (-1.053)
          , SP.c SP.Rel (-0.472) 0.0 (-0.745) (-0.556) (-0.5) (-0.96)
          , SP.a SP.Rel 8.95 8.95 0.0 SP.Arc0 SP.Sweep0 0.303 (-0.54)
          ]
      ]
  ]

downvoteIconSolid ∷ forall a b. HH.HTML a b
downvoteIconSolid = Svg.svg
  [ SP.class_ (H.ClassName "size-6")
  , SP.viewBox 0.0 0.0 24.0 24.0
  , SP.fill (Named "currentColor")
  ]
  [ Svg.path
      [ SP.d
          [ SP.m SP.Abs 15.73 5.5
          , SP.h SP.Rel 1.035
          , SP.a SP.Abs 7.465 7.465 0.0 SP.Arc0 SP.Sweep1 18.0 9.625
          , SP.a SP.Rel 7.465 7.465 0.0 SP.Arc0 SP.Sweep1 (-1.235) 4.125
          , SP.h SP.Rel (-0.148)
          , SP.c SP.Rel (-0.806) 0.0 (-1.534) 0.446 (-2.031) 1.08
          , SP.a SP.Rel 9.04 9.04 0.0 SP.Arc0 SP.Sweep1 (-2.861) 2.4
          , SP.c SP.Rel (-0.723) 0.384 (-1.35) 0.956 (-1.653) 1.715
          , SP.a SP.Rel 4.499 4.499 0.0 SP.Arc0 SP.Sweep0 (-0.322) 1.672
          , SP.v SP.Rel 0.633
          , SP.a SP.Abs 0.75 0.75 0.0 SP.Arc0 SP.Sweep1 9.0 22.0
          , SP.a SP.Rel 2.25 2.25 0.0 SP.Arc0 SP.Sweep1 (-2.25) (-2.25)
          , SP.c SP.Rel 0.0 (-1.152) 0.26 (-2.243) 0.723 (-3.218)
          , SP.c SP.Rel 0.266 (-0.558) (-0.107) (-1.282) (-0.725) (-1.282)
          , SP.h SP.Abs 3.622
          , SP.c SP.Rel (-1.026) 0.0 (-1.945) (-0.694) (-2.054) (-1.715)
          , SP.a SP.Abs 12.137 12.137 0.0 SP.Arc0 SP.Sweep1 1.5 12.25
          , SP.c SP.Rel 0.0 (-2.848) 0.992 (-5.464) 2.649 (-7.521)
          , SP.c SP.Abs 4.537 4.247 5.136 4.0 5.754 4.0
          , SP.h SP.Abs 9.77
          , SP.a SP.Rel 4.5 4.5 0.0 SP.Arc0 SP.Sweep1 1.423 0.23
          , SP.l SP.Rel 3.114 1.04
          , SP.a SP.Rel 4.5 4.5 0.0 SP.Arc0 SP.Sweep0 1.423 0.23
          , SP.z
          , SP.m SP.Abs 21.669 14.023
          , SP.c SP.Rel 0.536 (-1.362) 0.831 (-2.845) 0.831 (-4.398)
          , SP.c SP.Rel 0.0 (-1.22) (-0.182) (-2.398) (-0.52) (-3.507)
          , SP.c SP.Rel (-0.26) (-0.85) (-1.084) (-1.368) (-1.973) (-1.368)
          , SP.h SP.Abs 19.1
          , SP.c SP.Rel (-0.445) 0.0 (-0.72) 0.498 (-0.523) 0.898
          , SP.c SP.Rel 0.591 1.2 0.924 2.55 0.924 3.977
          , SP.a SP.Rel 8.958 8.958 0.0 SP.Arc0 SP.Sweep1 (-1.302) 4.666
          , SP.c SP.Rel (-0.245) 0.403 0.028 0.959 0.5 0.959
          , SP.h SP.Rel 1.053
          , SP.c SP.Rel 0.832 0.0 1.612 (-0.453) 1.918 (-1.227)
          , SP.z
          ]
      ]
  ]

upvoteIcon :: forall a b. HH.HTML a b
upvoteIcon = Svg.svg
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
          [ SP.m SP.Abs 6.633 10.25
          , SP.c SP.Rel 0.806 0.0 1.533 (-0.446) 2.031 (-1.08)
          , SP.a SP.Rel 9.041 9.041 0.0 SP.Arc0 SP.Sweep1 2.861 (-2.4)
          , SP.c SP.Rel 0.723 (-0.384) 1.35 (-0.956) 1.653 (-1.715)
          , SP.a SP.Rel 4.498 4.498 0.0 SP.Arc0 SP.Sweep0 0.322 (-1.672)
          , SP.v SP.Abs 2.75
          , SP.a SP.Rel 0.75 0.75 0.0 SP.Arc0 SP.Sweep1 0.75 (-0.75)
          , SP.a SP.Rel 2.25 2.25 0.0 SP.Arc0 SP.Sweep1 2.25 2.25
          , SP.c SP.Rel 0.0 1.152 (-0.26) 2.243 (-0.723) 3.218
          , SP.c SP.Rel (-0.266) 0.558 0.107 1.282 0.725 1.282
          , SP.m SP.Rel 0.0 0.0
          , SP.h SP.Rel 3.126
          , SP.c SP.Rel 1.026 0.0 1.945 0.694 2.054 1.715
          , SP.c SP.Rel 0.045 0.422 0.068 0.85 0.068 1.285
          , SP.a SP.Rel 11.95 11.95 0.0 SP.Arc0 SP.Sweep1 (-2.649) 7.521
          , SP.c SP.Rel (-0.388) 0.482 (-0.987) 0.729 (-1.605) 0.729
          , SP.h SP.Abs 13.48
          , SP.c SP.Rel (-0.483) 0.0 (-0.964) (-0.078) (-1.423) (-0.23)
          , SP.l SP.Rel (-3.114) (-1.04)
          , SP.a SP.Rel 4.501 4.501 0.0 SP.Arc0 SP.Sweep0 (-1.423) (-0.23)
          , SP.h SP.Abs 5.904
          , SP.m SP.Rel 10.598 (-9.75)
          , SP.h SP.Abs 14.25
          , SP.m SP.Abs 5.904 18.5
          , SP.c SP.Rel 0.083 0.205 0.173 0.405 0.27 0.602
          , SP.c SP.Rel 0.197 0.4 (-0.078) 0.898 (-0.523) 0.898
          , SP.h SP.Rel (-0.908)
          , SP.c SP.Rel (-0.889) 0.0 (-1.713) (-0.518) (-1.972) (-1.368)
          , SP.a SP.Rel 12.0 12.0 0.0 SP.Arc0 SP.Sweep1 (-0.521) (-3.507)
          , SP.c SP.Rel 0.0 (-1.553) 0.295 (-3.036) 0.831 (-4.398)
          , SP.c SP.Abs 3.387 9.953 4.167 9.5 5.0 9.5
          , SP.h SP.Rel 1.053
          , SP.c SP.Rel 0.472 0.0 0.745 0.556 0.5 0.96
          , SP.a SP.Rel 8.958 8.958 0.0 SP.Arc0 SP.Sweep0 (-1.302) 4.665
          , SP.c SP.Rel 0.0 1.194 0.232 2.333 0.654 3.375
          , SP.z
          ]
      ]
  ]

upvoteIconSolid :: forall a b. HH.HTML a b
upvoteIconSolid = Svg.svg
  [ SP.class_ (H.ClassName "size-6")
  , SP.viewBox 0.0 0.0 24.0 24.0
  , SP.fill (Named "currentColor")
  ]
  [ Svg.path
      [ SP.d
          [ SP.m SP.Abs 7.493 18.5
          , SP.c SP.Rel (-0.425) 0.0 (-0.82) (-0.236) (-0.975) (-0.632)
          , SP.a SP.Abs 7.48 7.48 0.0 SP.Arc0 SP.Sweep1 6.0 15.125
          , SP.c SP.Rel 0.0 (-1.75) 0.599 (-3.358) 1.602 (-4.634)
          , SP.c SP.Rel 0.151 (-0.192) 0.373 (-0.309) 0.6 (-0.397)
          , SP.c SP.Rel 0.473 (-0.183) 0.89 (-0.514) 1.212 (-0.924)
          , SP.a SP.Rel 9.042 9.042 0.0 SP.Arc0 SP.Sweep1 2.861 (-2.4)
          , SP.c SP.Rel 0.723 (-0.384) 1.35 (-0.956) 1.653 (-1.715)
          , SP.a SP.Rel 4.498 4.498 0.0 SP.Arc0 SP.Sweep0 0.322 (-1.672)
          , SP.v SP.Abs 2.75
          , SP.a SP.Abs 0.75 0.75 0.0 SP.Arc0 SP.Sweep1 15.0 2.0
          , SP.a SP.Rel 2.25 2.25 0.0 SP.Arc0 SP.Sweep1 2.25 2.25
          , SP.c SP.Rel 0.0 1.152 (-0.26) 2.243 (-0.723) 3.218
          , SP.c SP.Rel (-0.266) 0.558 0.107 1.282 0.725 1.282
          , SP.h SP.Rel 3.126
          , SP.c SP.Rel 1.026 0.0 1.945 0.694 2.054 1.715
          , SP.c SP.Rel 0.045 0.422 0.068 0.85 0.068 1.285
          , SP.a SP.Rel 11.95 11.95 0.0 SP.Arc0 SP.Sweep1 (-2.649) 7.521
          , SP.c SP.Rel (-0.388) 0.482 (-0.987) 0.729 (-1.605) 0.729
          , SP.h SP.Abs 14.23
          , SP.c SP.Rel (-0.483) 0.0 (-0.964) (-0.078) (-1.423) (-0.23)
          , SP.l SP.Rel (-3.114) (-1.04)
          , SP.a SP.Rel 4.501 4.501 0.0 SP.Arc0 SP.Sweep0 (-1.423) (-0.23)
          , SP.h SP.Rel (-0.777)
          , SP.z
          , SP.m SP.Abs 2.331 10.727
          , SP.a SP.Rel 11.969 11.969 0.0 SP.Arc0 SP.Sweep0 (-0.831) 4.398
          , SP.a SP.Rel 12.0 12.0 0.0 SP.Arc0 SP.Sweep0 0.52 3.507
          , SP.c SP.Abs 2.28 19.482 3.105 20.0 3.994 20.0
          , SP.h SP.Abs 4.9
          , SP.c SP.Rel 0.445 0.0 0.72 (-0.498) 0.523 (-0.898)
          , SP.a SP.Rel 8.963 8.963 0.0 SP.Arc0 SP.Sweep1 (-0.924) (-3.977)
          , SP.c SP.Rel 0.0 (-1.708) 0.476 (-3.305) 1.302 (-4.666)
          , SP.c SP.Rel 0.245 (-0.403) (-0.028) (-0.959) (-0.5) (-0.959)
          , SP.h SP.Abs 4.25
          , SP.c SP.Rel (-0.832) 0.0 (-1.612) 0.453 (-1.918) 1.227
          , SP.z
          ]
      ]
  ]