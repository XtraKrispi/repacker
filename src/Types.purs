module Types where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)
import Supabase.Auth (UserEmail(..))
import Supabase.Auth.Types (UserId(..))

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

type BoardGame = { thumbnailUrl :: String, imageUrl :: String | BoardGameBase }