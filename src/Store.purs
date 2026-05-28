module Store where

import Prelude

import Data.Array (filter)
import Data.Generic.Rep (class Generic)
import Types (Key)

data ToastKey

data Severity
  = Info
  | Success
  | Warning
  | Error

derive instance Eq Severity

type Toast =
  { message :: String
  , severity :: Severity
  , key :: Key (ToastKey)
  }

type Store = { toasts :: Array Toast }

initialStore :: Store
initialStore = { toasts: [] }

data Action
  = AddToast Toast
  | RemoveToast (Key ToastKey)

reduce :: Store -> Action -> Store
reduce store (AddToast toast) = store { toasts = store.toasts <> [ toast ] }
reduce store (RemoveToast toastKey) = store { toasts = filter (\t -> t.key /= toastKey) store.toasts }