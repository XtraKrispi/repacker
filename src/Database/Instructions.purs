module Database.Instructions
  ( fetchInstructions
  , newInstructions
  , InstructionsSaveError(..)
  ) where

import Prelude

import Data.Array (catMaybes, null)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.MediaType.Common (imageGIF, imageJPEG, imagePNG)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Data.UUID (toString)
import Effect.Aff (Aff)
import Supabase (Client, Response, StoragePath(..), Table, eq_, from, fromStorage, insert, mkTable, run, select, upload)
import Supabase.Auth.Types (UserId)
import Supabase.Types (BucketName(..))
import Supabase.UUID (UUID)
import Types (GameId, ImageKey, Instructions, InstructionsKey, Key)
import Web.File.File (File)
import Web.File.File as File

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

fetchInstructions :: Client -> GameId -> Aff (Array Instructions)
fetchInstructions client gameId = do
  results <- client
    # from instructionsTable
    # select
    # eq_ @"bgg_id" (unwrap gameId)
    # run
  pure $ catMaybes $ toInstructions <$> fromMaybe [] results.data

{- Workflow:
   - Create the instructions entry
   - Upload files to storage for each step -> filename is the uuid
-}

data InstructionsSaveError = FailedToSave String | ImagesFailedToUpload (Array (Tuple ImageKey String))

newInstructions :: Client -> GameId -> InstructionsKey -> Instructions -> Array (Tuple ImageKey File) -> Aff (Either InstructionsSaveError Unit)
newInstructions client gameId instructionsKey instructions images = do
  results <- client # from instructionsTable # insert (toDbInstructions gameId instructionsKey instructions) # run
  case results.error of
    Nothing -> do
      responses <- traverse (uploadStepImage client) $ images
      let errors = filterMap (\(k /\ resp) -> (\err -> k /\ err.message) <$> resp.error) responses
      if null errors then
        pure $ Right unit
      else
        pure $ Left $ ImagesFailedToUpload errors
    Just err -> pure $ Left $ FailedToSave err.message

uploadStepImage :: Client -> Tuple ImageKey File -> Aff (Tuple ImageKey (Response { path :: String }))
uploadStepImage client (imageKey /\ file) = (\d -> imageKey /\ d) <$> upload (StoragePath (toString (unwrap imageKey) <> extension file)) file { upsert: true } (fromStorage (BucketName "images") client)

extension :: File -> String
extension file = case File.type_ file of
  Just ext
    | ext == imageGIF -> ".gif"
    | ext == imageJPEG -> ".jpg"
    | ext == imagePNG -> ".png"
  _ -> ""

toInstructions
  :: {
     | DbInstructionsRow
     }
  -> Maybe Instructions
toInstructions instructions = Nothing

toDbInstructions :: GameId -> InstructionsKey -> Instructions -> { | DbInstructionsForInsertRow }
toDbInstructions gameId instructionsKey instructions =
  { bgg_id: unwrap gameId
  , data: instructions
  , instructions_key: wrap $ unwrap instructionsKey
  }