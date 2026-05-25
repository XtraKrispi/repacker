module Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Supabase.Auth (UserEmail)
import Supabase.Auth.Types (UserId)
import Yoga.JSON (class ReadForeign, class WriteForeign)
import Yoga.JSON.Generics (genericReadForeignTaggedSum, genericWriteForeignTaggedSum)
import Yoga.JSON.Generics.TaggedSumRep as TaggedSum

newtype GameId = GameId String

derive instance newtypeGameId :: Newtype GameId _
derive instance eqGameId :: Eq GameId
derive instance genericGameId :: Generic GameId _
instance showRoute :: Show GameId where
  show = genericShow

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
  , expansions :: Array { gameId :: GameId, title :: String }
  | BoardGameBase
  }

type Profile = { firstName :: String, lastName :: String }

type Instructions =
  { description :: String
  , bggId :: GameId
  , creator :: UserId
  , allowsSleeves :: Boolean
  , requiresBaggies :: Boolean
  , steps :: Array PackingStep
  , includedExpansions :: Array GameId
  , otherMaterials :: Array OtherMaterials
  }

type PackingStep =
  { description :: String
  , imagePath :: String
  , stepOrdinal :: Int
  }

type Url = String

data OtherMaterials
  = CustomInsert Url
  | CustomBox Url
  | OtherMaterials String

derive instance Generic OtherMaterials _

instance WriteForeign OtherMaterials where
  writeImpl = genericWriteForeignTaggedSum TaggedSum.defaultOptions

instance ReadForeign OtherMaterials where
  readImpl = genericReadForeignTaggedSum TaggedSum.defaultOptions