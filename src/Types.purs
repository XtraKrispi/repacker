module Types where

import Prelude

import Control.Monad.Except (Except, ExceptT(..))
import Data.Either (note)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.List.NonEmpty (NonEmptyList, singleton)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Set (Set, toUnfoldable)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID, parseUUID, toString)
import Foreign (Foreign, ForeignError(..))
import Supabase.Auth (UserEmail)
import Supabase.Auth.Types (UserId)
import Web.File.File (File)
import Yoga.JSON (class ReadForeign, class WriteForeign, readImpl, writeImpl)

newtype GameId = GameId String

derive instance newtypeGameId :: Newtype GameId _
derive instance eqGameId :: Eq GameId
derive instance ordGameId :: Ord GameId
derive instance genericGameId :: Generic GameId _
instance showRoute :: Show GameId where
  show = genericShow

instance WriteForeign GameId where
  writeImpl (GameId gameId) = writeImpl gameId

instance ReadForeign GameId where
  readImpl = map wrap <<< readImpl

type SessionInfo =
  { email :: UserEmail
  , userId :: UserId
  , name :: Maybe String
  }

type BoardGameBase =
  ( bggId :: GameId
  , title :: String
  , yearPublished :: Maybe Int
  )

type BoardGameSummary = { | BoardGameBase }

type BoardGame =
  { thumbnailUrl :: String
  , imageUrl :: String
  , expansions ::
      Array
        { gameId :: GameId
        , title :: String
        }
  | BoardGameBase
  }

type Profile =
  { firstName :: String
  , lastName :: String
  }

newtype IncludedExpansions = IncludedExpansions (Set GameId)

derive instance Newtype IncludedExpansions _

instance WriteForeign IncludedExpansions where
  writeImpl (IncludedExpansions set) = writeImpl (toUnfoldable set :: Array GameId)

instance ReadForeign IncludedExpansions where
  readImpl = map wrap <<< readImpl

type Instructions =
  { description :: String
  , allowsSleeves :: Boolean
  , requiresBaggies :: Boolean
  , customInsert :: Url
  , steps :: Array PackingStep
  , includedExpansions :: IncludedExpansions
  , otherMaterials :: String
  }

type PackingStep =
  { description :: String
  , image :: Maybe ImageKey
  , stepOrdinal :: Int
  }

type ImageKey = Key Image

data Image

type Url = String

newtype Key :: forall k. k -> Type
newtype Key a = Key UUID

derive instance Newtype (Key a) _
derive instance Eq (Key a)
derive instance Ord (Key a)

instance WriteForeign (Key a) where
  writeImpl = writeImpl <<< toString <<< unwrap

instance ReadForeign (Key a) where
  readImpl :: Foreign -> Except (NonEmptyList ForeignError) (Key a)
  readImpl f = do
    str <- readImpl f
    uuid <- (\muuid -> ExceptT (Identity $ note (singleton (ForeignError "Invalid UUID")) muuid)) $ parseUUID str
    pure $ wrap uuid

type InstructionsKey = Key Instructions

