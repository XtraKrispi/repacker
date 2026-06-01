module Route where

import Prelude

import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Newtype (unwrap)
import Data.Show.Generic (genericShow)
import Data.UUID (parseUUID, toString)
import Data.UUID as UUID
import Effect.Class (class MonadEffect, liftEffect)
import Routing.Duplex (RouteDuplex', as, default, print, root, segment)
import Routing.Duplex.Generic as G
import Routing.Duplex.Generic.Syntax as D
import Routing.Hash (setHash)
import Supabase.Auth.Types (UserId(..))
import Supabase.UUID (UUID(..))
import Types (GameId(..), InstructionsKey, Key(..))

data Route
  = HomeR
  | GameR GameId
  | NewInstructionsR GameId
  | UpdateInstructionsR GameId InstructionsKey
  | ViewInstructionsR GameId InstructionsKey
  | ProfileR UserId

derive instance genericRoute :: Generic Route _
derive instance eqRoute :: Eq Route

instance showRoute :: Show Route where
  show = genericShow

routeCodec :: RouteDuplex' Route
routeCodec = default HomeR $ root $ G.sum
  { "HomeR": G.noArgs
  , "GameR": "game" D./ gameId segment
  , "NewInstructionsR": "game" D./ gameId segment D./ "new"
  , "UpdateInstructionsR": "game" D./ gameId segment D./ key segment
  , "ViewInstructionsR": "game" D./ gameId segment D./ "view" D./ key segment
  , "ProfileR": "profile" D./ userId segment
  }

userId :: RouteDuplex' String -> RouteDuplex' UserId
userId = as (UUID.toString <<< unwrap <<< unwrap) (map (UserId <<< UUID) <<< note "Invalid UUID" <<< UUID.parseUUID)

gameId :: RouteDuplex' String -> RouteDuplex' GameId
gameId = as unwrap (pure <<< GameId)

key :: forall a. RouteDuplex' String -> RouteDuplex' (Key a)
key = as (toString <<< unwrap) (\s -> note "Invalid Key" $ map Key $ parseUUID s)

-- | sets Window.location
navigate :: forall m. MonadEffect m => Route -> m Unit
navigate r = liftEffect $ setHash (print routeCodec r)
