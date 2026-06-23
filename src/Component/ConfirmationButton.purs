module Component.ConfirmationButton where

import Prelude

import DOM.HTML.Indexed.FormMethod (FormMethod(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import FFI.Dialog (close, openModal)
import Halogen (get, modify_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Helpers (randomString)

type CoreData =
  ( buttonText :: String
  , buttonCss :: H.ClassName
  , modalContent :: String
  )

type Input = { | CoreData }

data State
  = GettingId { | CoreData }
  | Initialized { elementId :: String | CoreData }

data Output = Confirmed

data Action = Initialize | OpenDialog | NoClicked | YesClicked

component :: forall query m. MonadAff m => MonadEffect m => H.Component query Input Output m
component = H.mkComponent { initialState, eval: H.mkEval H.defaultEval { handleAction = handleAction, initialize = Just Initialize }, render }

initialState :: Input -> State
initialState { buttonText, buttonCss, modalContent } = GettingId { buttonText, buttonCss, modalContent }

handleAction :: forall slots m. MonadAff m => MonadEffect m => Action -> H.HalogenM State Action slots Output m Unit
handleAction Initialize = do
  elementId <- liftEffect $ randomString 8 $ toCharArray "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  modify_ \state ->
    case state of
      GettingId { buttonText, buttonCss, modalContent } -> Initialized { buttonText, buttonCss, modalContent, elementId }
      Initialized { buttonText, buttonCss, modalContent } -> Initialized { buttonText, buttonCss, modalContent, elementId }
handleAction OpenDialog = do
  st <- get
  case st of
    Initialized { elementId } ->
      liftEffect $ openModal ("#" <> elementId)
    _ -> pure unit
handleAction NoClicked = do
  st <- get
  case st of
    Initialized { elementId } -> liftEffect $ close ("#" <> elementId)
    _ -> pure unit
handleAction YesClicked = do
  st <- get
  case st of
    Initialized { elementId } -> do
      H.raise Confirmed
      liftEffect $ close ("#" <> elementId)
    _ -> pure unit

render :: forall slots m. MonadAff m => MonadEffect m => State -> H.ComponentHTML Action slots m
render (GettingId _) = HH.text ""
render (Initialized { buttonText, buttonCss, modalContent, elementId }) = HH.div []
  [ HH.button [ HP.class_ buttonCss, HE.onClick (\_ -> OpenDialog) ]
      [ HH.text buttonText
      ]
  , HH.dialog
      [ HP.class_ (H.ClassName "modal")
      , HP.id elementId
      ]
      [ HH.div [ HP.class_ (H.ClassName "modal-box") ]
          [ HH.form [ HP.method Dialog ]
              [ HH.button [ HP.class_ (H.ClassName "btn btn-sm btn-circle btn-ghost absolute right-2 top-2") ] [ HH.text "✕" ]
              ]
          , HH.h3 [ HP.class_ (H.ClassName "text-lg font-bold") ]
              [ HH.text "Are you sure?" ]
          , HH.p [ HP.class_ (H.ClassName "py-4") ]
              [ HH.text modalContent ]
          , HH.div [ HP.class_ (H.ClassName "modal-action") ]
              [ HH.button [ HE.onClick (\_ -> NoClicked), HP.class_ (H.ClassName "btn btn-secondary") ] [ HH.text "No" ]
              , HH.button [ HE.onClick (\_ -> YesClicked), HP.class_ (H.ClassName "btn btn-primary") ] [ HH.text "Yes" ]
              ]
          ]
      , HH.form
          [ HP.method Dialog
          , HP.class_ (H.ClassName "modal-backdrop")
          ]
          [ HH.button_ [ HH.text "close" ] ]
      ]
  ]
