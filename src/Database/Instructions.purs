module Database.Instructions where

import Prelude

import Data.Array (catMaybes)
import Data.DateTime (DateTime)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Supabase (Client, Rel, Table, UUID, eq_, from, mkTable, run, selectColumns)
import Supabase.Auth.Types (UserId)
import Types (GameId, Instructions)

type DbInstructionsRow =
  ( id :: Int
  , bgg_id :: String
  , description :: String
  , created_by :: UserId
  , created_at :: DateTime
  , allows_sleeves :: Boolean
  , requires_baggies :: Boolean
  , included_expansions :: Array String
  , other_materials :: Array String
  , custom_insert_url :: Maybe String
  )

type DbInstructionsRelations = (steps :: Rel DbStepsRow ())

type DbStepsRow =
  ( id :: Int
  , instructions_id :: Int
  , description :: String
  , image_id :: UUID
  , ordinal :: Int
  )

instructionsTable :: Table DbInstructionsRow () DbInstructionsRelations
instructionsTable = mkTable "instructions"

stepsTable :: Table DbStepsRow () ()
stepsTable = mkTable "steps"

fetchInstructions :: Client -> GameId -> Aff (Array (Tuple Int Instructions))
fetchInstructions client gameId = do
  results <- client
    # from instructionsTable
    # selectColumns @"id, bgg_id, description, created_by, created_at, allows_sleeves, requires_baggies, included_expansions, other_materials, custom_insert_url, steps(id, instructions_id, description, image_id, ordinal)"
    # eq_ @"bgg_id" (unwrap gameId)
    # run
  pure $ catMaybes $ toInstructions <$> fromMaybe [] results.data

toInstructions
  :: { steps ::
         Array
           { | DbStepsRow }

     | DbInstructionsRow
     }
  -> Maybe (Tuple Int Instructions)
toInstructions instructions = Nothing