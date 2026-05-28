module Store where

import Prelude

import Data.Array (filter)
import Types (Key)

data ToastKey

type Toast = { message :: String, key :: Key (ToastKey) }

type Store = { toasts :: Array Toast }

initialStore :: Store
initialStore = { toasts: [] }

data Action = AddToast Toast | RemoveToast (Key ToastKey)

reduce :: Store -> Action -> Store
reduce store (AddToast toast) = store { toasts = store.toasts <> [ toast ] }
reduce store (RemoveToast toastKey) = store { toasts = filter (\t -> t.key /= toastKey) store.toasts }