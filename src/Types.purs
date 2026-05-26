module Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Set (Set)
import Data.Show.Generic (genericShow)
import Data.UUID (UUID)
import Supabase.Auth (UserEmail)
import Supabase.Auth.Types (UserId)

newtype GameId = GameId String

derive instance newtypeGameId :: Newtype GameId _
derive instance eqGameId :: Eq GameId
derive instance ordGameId :: Ord GameId
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
  , customInsert :: Url
  , steps :: Array PackingStep
  , includedExpansions :: Set GameId
  , otherMaterials :: String

  }

type PackingStep =
  { description :: String
  , image :: Maybe Image
  , stepOrdinal :: Int
  }

type Image = { imageId :: UUID, imageContent :: String }

type Url = String

