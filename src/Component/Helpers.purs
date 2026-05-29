module Component.Helpers where

import Prelude

import Data.Array (filter, intercalate)
import Data.Newtype (wrap)
import Data.Tuple (Tuple)
import Data.Tuple as Tuple
import Data.UUID (genUUID)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Store.Monad (class MonadStore, updateStore)
import Store (ToastMessage)
import Store as S
import Web.HTML.Common (ClassName(..))

classList :: Array (Tuple String Boolean) -> ClassName
classList = ClassName <<< intercalate " " <<< map Tuple.fst <<< filter Tuple.snd

addToast :: forall m. MonadEffect m => MonadStore S.Action S.Store m => ToastMessage -> m Unit
addToast { message, severity } = do
  key <- wrap <$> liftEffect genUUID
  updateStore $ S.AddToast { message, severity, key }