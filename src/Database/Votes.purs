module Database.Votes where

import Prelude

import Supabase.Auth.Types (UserId)

type DbVotesRow =
  ( instructions_id :: Int
  , vote_type :: String
  , voted_by :: UserId
  )