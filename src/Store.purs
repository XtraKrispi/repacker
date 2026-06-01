module Store where

import Prelude

import Data.Array (filter)
import Types (Key)

-- TODO: Add session to the store... and wire up with receive so that we automatically update the views when the user logs in or out

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

type Store = { toasts :: Array Toast }

initialStore :: Store
initialStore = { toasts: [] }

data Action
  = AddToast Toast
  | RemoveToast (Key ToastKey)

reduce :: Store -> Action -> Store
reduce store (AddToast toast) = store { toasts = store.toasts <> [ toast ] }
reduce store (RemoveToast toastKey) = store { toasts = filter (\t -> t.key /= toastKey) store.toasts }