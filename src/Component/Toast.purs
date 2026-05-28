module Component.Toast where

import Prelude

import Data.Array ((\\))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import Effect.Aff (Milliseconds(..))
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Halogen (get, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Store.Connect (Connected, connect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Halogen.Store.Select (Selector, selectEq)
import Halogen.Svg.Attributes (Color(..))
import Halogen.Svg.Attributes as SP
import Halogen.Svg.Attributes.Path (CommandArcChoice(..), CommandPositionReference(..), CommandSweepChoice(..), a, c, h, l, m, v, z)
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(..))
import Halogen.Svg.Attributes.StrokeLineJoin (StrokeLineJoin(..))
import Halogen.Svg.Elements as Svg
import Store (Severity(..), Toast)
import Store as S

type State = { toasts :: Array Toast }

selectToasts :: Selector S.Store (Array Toast)
selectToasts = selectEq \store -> store.toasts

data Action = Receive (Connected (Array Toast) Unit)

component
  :: forall query output m
   . MonadAff m
  => MonadEffect m
  => MonadStore S.Action S.Store m
  => H.Component query Unit output m
component = connect selectToasts $ H.mkComponent
  { initialState
  , eval: H.mkEval H.defaultEval
      { handleAction = handleAction
      , receive = Just <<< Receive
      }
  , render
  }

initialState :: Connected (Array Toast) Unit -> State
initialState { context } = { toasts: context }

handleAction :: forall slots output m. MonadAff m => MonadEffect m => MonadStore S.Action S.Store m => Action -> H.HalogenM State Action slots output m Unit
handleAction (Receive { context }) = do
  { toasts } <- get
  let newToasts = context \\ toasts
  modify_ _ { toasts = context }
  liftAff $ Aff.delay (Milliseconds 3000.0)
  traverse_ (\t -> updateStore (S.RemoveToast t.key)) newToasts

render
  :: forall slots m
   . MonadAff m
  => MonadEffect m
  => MonadStore S.Action S.Store m
  => State
  -> H.ComponentHTML Action slots m
render { toasts } = HH.div [ HP.class_ (H.ClassName "toast toast-center") ] (map renderToast toasts)

renderToast :: forall slots m. Toast -> H.ComponentHTML Action slots m
renderToast { message, severity } = case severity of
  Info -> infoAlert message
  Success -> successAlert message
  Warning -> warningAlert message
  Error -> errorAlert message

infoAlert :: forall slots m. String -> H.ComponentHTML Action slots m
infoAlert message =
  HH.div
    [ HP.attr (H.AttrName "role") "alert"
    , HP.class_ (H.ClassName "alert alert-info")
    ]
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
                [ m Abs 13.0 16.0
                , h Rel (-1.0)
                , v Rel (-4.0)
                , h Rel (-1.0)
                , m Rel 1.0 (-4.0)
                , h Rel 0.01
                , m Abs 21.0 12.0
                , a Rel 9.0 9.0 0.0 Arc1 Sweep1 (-18.0) 0.0
                , a Rel 9.0 9.0 0.0 Arc0 Sweep1 18.0 0.0
                , z
                ]
            ]
        ]
    , HH.span_ [ HH.text message ]
    ]

successAlert :: forall slots m. String -> H.ComponentHTML Action slots m
successAlert message =
  HH.div
    [ HP.attr (H.AttrName "role") "alert"
    , HP.class_ (H.ClassName "alert alert-success")
    ]
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
                [ m Abs 9.0 12.0
                , l Rel 2.0 2.0
                , l Rel 4.0 (-4.0)
                , m Rel 6.0 2.0
                , a Rel 9.0 9.0 0.0 Arc1 Sweep1 (-18.0) 0.0
                , a Rel 9.0 9.0 0.0 Arc0 Sweep1 18.0 0.0
                , z
                ]
            ]
        ]
    , HH.span_ [ HH.text message ]
    ]

warningAlert :: forall slots m. String -> H.ComponentHTML Action slots m
warningAlert message =
  HH.div
    [ HP.attr (H.AttrName "role") "alert"
    , HP.class_ (H.ClassName "alert alert-warning")
    ]
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
                [ m Abs 12.0 9.0
                , v Rel 2.0
                , m Rel 0.0 4.0
                , h Rel 0.01
                , m Rel (-6.938) 4.0
                , h Rel 13.856
                , c Rel 1.54 0.0 2.502 (-1.667) 1.732 (-3.0)
                , l Abs 13.732 4.0
                , c Rel (-0.77) (-1.333) (-2.694) (-1.333) (-3.464) 0.0
                , l Abs 3.34 16.0
                , c Rel (-0.77) 1.333 0.192 3.0 1.732 3.0
                , z
                ]
            ]
        ]
    , HH.span_ [ HH.text message ]
    ]

errorAlert :: forall slots m. String -> H.ComponentHTML Action slots m
errorAlert message =
  HH.div
    [ HP.attr (H.AttrName "role") "alert"
    , HP.class_ (H.ClassName "alert alert-error")
    ]
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
                [ m Abs 10.0 14.0
                , l Rel 2.0 (-2.0)
                , m Rel 0.0 0.0
                , l Rel 2.0 (-2.0)
                , m Rel (-2.0) 2.0
                , l Rel (-2.0) (-2.0)
                , m Rel 2.0 2.0
                , l Rel 2.0 2.0
                , m Rel 7.0 (-2.0)
                , a Rel 9.0 9.0 0.0 Arc1 Sweep1 (-18.0) 0.0
                , a Rel 9.0 9.0 0.0 Arc0 Sweep1 18.0 0.0
                , z
                ]
            ]
        ]
    , HH.span_ [ HH.text message ]
    ]