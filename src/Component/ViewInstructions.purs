module Component.ViewInstructions where

import Prelude

import Bgg (bggThing)
import Data.Array (catMaybes, findMap, fromFoldable, index, length, mapWithIndex, null)
import Data.Either (Either(..))
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Database.Instructions (fetchImagesForInstructions, fetchSingleInstructions)
import Database.Profile (fetchProfile)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Halogen (get, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (ButtonType(..))
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SP
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(..))
import Halogen.Svg.Attributes.StrokeLineJoin (StrokeLineJoin(..))
import Halogen.Svg.Elements as Svg
import Network.RemoteData (RemoteData(..), withDefault)
import Network.RemoteData as RemoteData
import Route (Route(..), routeCodec)
import Routing.Duplex (print)
import Supabase (Client)
import Types (BoardGame, FileContents, GameId, Image(..), Images, IncludedExpansions(..), Instructions, InstructionsKey, PackingStep, Profile, SessionInfo, InstructionsWithUser)

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

component :: forall query output m. MonadEffect m => MonadAff m => H.Component query Input output m
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

handleAction :: forall slots output m. MonadEffect m => MonadAff m => Action -> H.HalogenM State Action slots output m Unit
handleAction Initialize = do
  { client, instructionsKey, gameId } <- get
  modify_ _
    { instructions = Loading
    , game = Loading
    , authorProfile = Loading
    }
  instructions <- liftAff $ fetchSingleInstructions client instructionsKey
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

render :: forall slots m. MonadAff m => MonadEffect m => State -> H.ComponentHTML Action slots m
render state = case state.game /\ state.instructions /\ state.authorProfile of
  (Success game /\ Success { createdBy, instructions } /\ Success author) -> HH.div [ HP.class_ (H.ClassName "flex flex-col gap-6 max-w-4xl mx-auto") ]
    [ renderMetadata author game instructions (editLink createdBy)
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
  where
  -- | Show an Edit link only when the logged-in user authored these instructions.
  editLink authorId
    | (_.userId <$> state.session) == Just authorId = HH.a
        [ HP.class_ (H.ClassName "btn btn-sm btn-secondary")
        , HP.href ("#" <> print routeCodec (UpdateInstructionsR state.gameId state.instructionsKey))
        ]
        [ HH.text "Edit" ]
    | otherwise = HH.text ""

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

renderMetadata :: forall slots m. MonadAff m => MonadEffect m => Maybe Profile -> BoardGame -> Instructions -> H.ComponentHTML Action slots m -> H.ComponentHTML Action slots m
renderMetadata author game instructions editLink = HH.div [ HP.class_ (H.ClassName "card bg-base-200 shadow-xl") ]
  [ HH.div [ HP.class_ (H.ClassName "card-body") ]
      [ HH.div [ HP.class_ (H.ClassName "flex justify-between items-start gap-4") ]
          [ HH.div [ HP.class_ (H.ClassName "flex flex-col gap-1") ]
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
              , HH.p [ HP.class_ (H.ClassName "text-sm text-base-content/60") ]
                  [ HH.text $ "Packing guide by " <> extractAuthorName author ]
              ]
          , editLink
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