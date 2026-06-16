module Store where

import Prelude

import Data.Array (filter)
import Data.Maybe (Maybe(..))
import Halogen.Store.Select (Selector, selectEq)
import Types (Environment, Key, SessionInfo)

data ToastKey

data Severity
  = Info
  | Success
  | Warning
  | Error

derive instance Eq Severity

type Toast = { key :: Key (ToastKey) | ToastMessageR }
type ToastMessage = { | ToastMessageR }
type ToastMessageR =
  ( message :: String
  , severity :: Severity
  )

type Store =
  { toasts :: Array Toast
  , session :: Maybe SessionInfo
  , environment :: Environment
  }

initialStore :: Environment -> Maybe SessionInfo -> Store
initialStore environment session =
  { toasts: []
  , session
  , environment
  }

data Action
  = AddToast Toast
  | RemoveToast (Key ToastKey)
  | LoginUser SessionInfo
  | LogoutUser

reduce :: Store -> Action -> Store
reduce store (AddToast toast) = store { toasts = store.toasts <> [ toast ] }
reduce store (RemoveToast toastKey) = store { toasts = filter (\t -> t.key /= toastKey) store.toasts }
reduce store (LoginUser sessionInfo) = store { session = Just sessionInfo }
reduce store LogoutUser = store { session = Nothing }

selectSession :: Selector Store (Maybe SessionInfo)
selectSession = selectEq _.session

selectSessionAndEnvironment :: Selector Store { session :: Maybe SessionInfo, environment :: Environment }
selectSessionAndEnvironment = selectEq (\store -> { session: store.session, environment: store.environment })
