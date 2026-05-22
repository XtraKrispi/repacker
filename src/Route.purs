module Route where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Effect.Class (class MonadEffect, liftEffect)
import Routing.Duplex (RouteDuplex', as, default, path, print, root, segment)
import Routing.Duplex.Generic as G
import Routing.Hash (setHash)
import Supabase.Auth (UserEmail(..))
import Types (GameId(..))

data Route
  = HomeR
  | GameR GameId
  | NewInstructionsR GameId
  | ProfileR UserEmail

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = default HomeR $ root $ G.sum
  { "HomeR": G.noArgs
  , "GameR": path "game" (gameId segment)
  , "NewInstructionsR": path "game" (path "new" (gameId segment))
  , "ProfileR": path "profile" (email segment)
  }

email :: RouteDuplex' String -> RouteDuplex' UserEmail
email = as unwrap (pure <<< UserEmail)

gameId :: RouteDuplex' String -> RouteDuplex' GameId
gameId = as unwrap (pure <<< GameId)

-- | sets Window.location
navigate :: forall m. MonadEffect m => Route -> m Unit
navigate r = liftEffect $ setHash (print routeCodec r)