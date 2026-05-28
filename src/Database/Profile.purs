module Database.Profile
  ( fetchProfile
  , saveProfile
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Supabase (Client, Table, eq_, from, maybeSingle, mkTable, select, upsertWith)
import Supabase.Auth.Types (UserId)
import Types (Profile)

type DbProfileRow = (user_id :: UserId, first_name :: String, last_name :: String)

type DbProfile = { | DbProfileRow }

type UpsertProfile = { onConflict :: String }

profilesTable :: Table DbProfileRow () ()
profilesTable = mkTable "profiles"

fetchProfile :: Client -> UserId -> Aff (Either String (Maybe Profile))
fetchProfile client userId = do
  results <- client # from profilesTable # select # eq_ @"user_id" userId # maybeSingle
  case results.error of
    Just err -> pure $ Left err.message
    Nothing -> pure $ Right $ toProfile <$> results.data

saveProfile :: Client -> UserId -> Profile -> Aff (Either String Unit)
saveProfile client userId profile = do
  results <- client # from profilesTable # upsertWith (fromProfile userId profile) { onConflict: "user_id" } # maybeSingle
  case results.error of
    Just err -> pure $ Left err.message
    Nothing -> pure $ Right unit

toProfile :: DbProfile -> Profile
toProfile { first_name, last_name } = { firstName: first_name, lastName: last_name }

fromProfile :: UserId -> Profile -> DbProfile
fromProfile userId { firstName, lastName } = { first_name: firstName, last_name: lastName, user_id: userId }