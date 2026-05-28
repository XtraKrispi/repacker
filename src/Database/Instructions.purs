module Database.Instructions
  ( fetchInstructions
  , newInstructions
  , InstructionsSaveError(..)
  ) where

import Prelude

import Data.Array (null)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.UUID (toString)
import Effect.Aff (Aff, catchError)
import Supabase (Client, StoragePath(..), Table, eq_, from, fromStorage, insert, mkTable, run, select, upload)
import Supabase.Auth.Types (UserId)
import Supabase.Types (BucketName(..))
import Supabase.UUID (UUID)
import Types (FileName, GameId, Instructions, InstructionsKey)
import Web.File.File (File)

type DbInstructionsRow =
  ( id :: Int
  , created_by :: UserId
  , created_at :: DateTime
  | DbInstructionsForInsertRow
  )

type DbInstructionsForInsertRow =
  ( bgg_id :: String
  , data :: Instructions
  , instructions_key :: UUID
  )

instructionsTable :: Table DbInstructionsRow () ()
instructionsTable = mkTable "instructions"

fetchInstructions :: Client -> GameId -> Aff (Array (Tuple UserId Instructions))
fetchInstructions client gameId = do
  results <- client
    # from instructionsTable
    # select
    # eq_ @"bgg_id" (unwrap gameId)
    # run
  pure $ toInstructions <$> fromMaybe [] results.data

data InstructionsSaveError = FailedToSave String | ImagesFailedToUpload (Array (Tuple FileName String))

newInstructions :: Client -> GameId -> InstructionsKey -> Instructions -> Array (Tuple FileName File) -> Aff (Either InstructionsSaveError Unit)
newInstructions client gameId instructionsKey instructions images = do
  results <- client # from instructionsTable # insert (toDbInstructions gameId instructionsKey instructions) # run
  case results.error of
    Nothing -> do
      responses <- traverse (uploadStepImage client instructionsKey) $ images
      let errors = filterMap (\(k /\ err) -> (k /\ _) <$> err) responses
      if null errors then
        pure $ Right unit
      else
        pure $ Left $ ImagesFailedToUpload errors
    Just err -> pure $ Left $ FailedToSave err.message

uploadStepImage :: Client -> InstructionsKey -> Tuple FileName File -> Aff (Tuple FileName (Maybe String))
uploadStepImage client instructionsKey (fileName /\ file) = do
  catchError
    ( (\d -> fileName /\ (_.message <$> d.error))
        <$> upload
          ( StoragePath
              ( toString (unwrap instructionsKey)
                  <> "/"
                  <> fileName
              )
          )
          file
          { upsert: true }
          (fromStorage (BucketName "images") client)

    )
    -- TODO: There is a bug where the response from supabase doesn't match what's expected
    -- We'll suppress the errors for now
    (\_err -> pure $ fileName /\ Nothing)

toDbInstructions :: GameId -> InstructionsKey -> Instructions -> { | DbInstructionsForInsertRow }
toDbInstructions gameId instructionsKey instructions =
  { bgg_id: unwrap gameId
  , data: instructions
  , instructions_key: wrap $ unwrap instructionsKey
  }

toInstructions :: { | DbInstructionsRow } -> Tuple UserId Instructions
toInstructions row = row.created_by /\ row.data