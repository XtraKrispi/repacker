module Database.Instructions where

import Prelude

import Data.Array (catMaybes)
import Data.DateTime (DateTime(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple)
import Effect.Aff (Aff)
import Supabase (Client, Rel, Table, UUID, eq_, from, mkTable, run, select, selectColumns)
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
  )

type DbInstructionsRelations = (steps :: Rel DbStepsRow (), other_materials :: Rel DbOtherMaterialsRow ())

type DbStepsRow =
  ( id :: Int
  , instructions_id :: Int
  , description :: String
  , image_id :: UUID
  , ordinal :: Int
  )

type DbOtherMaterialsRow =
  ( id :: Int
  , instructions_id :: Int
  , other_material_type :: String
  , details :: String
  )

instructionsTable :: Table DbInstructionsRow () DbInstructionsRelations
instructionsTable = mkTable "instructions"

stepsTable :: Table DbStepsRow () ()
stepsTable = mkTable "steps"

otherMaterialsTable :: Table DbOtherMaterialsRow () ()
otherMaterialsTable = mkTable "other_materials"

fetchInstructions :: Client -> GameId -> Aff (Array (Tuple Int Instructions))
fetchInstructions client gameId = do
  results <- client
    # from instructionsTable
    # selectColumns @"id, bgg_id, description, created_by, created_at, allows_sleeves, requires_baggies, included_expansions, steps(id, instructions_id, description, image_id, ordinal), other_materials(id, instructions_id, other_material_type, details)"
    # eq_ @"bgg_id" (unwrap gameId)
    # run
  pure $ catMaybes $ toInstructions <$> fromMaybe [] results.data

toInstructions
  :: { steps ::
         Array
           { | DbStepsRow }
     , other_materials :: Array { | DbOtherMaterialsRow }
     | DbInstructionsRow
     }
  -> Maybe (Tuple Int Instructions)
toInstructions instructions = Nothing