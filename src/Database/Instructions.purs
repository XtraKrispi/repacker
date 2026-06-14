module Database.Instructions
  ( fetchInstructions
  , newInstructions
  , fetchSingleInstructions
  , fetchImagesForInstructions
  , updateInstructions
  , InstructionsSaveError(..)
  ) where

import Prelude

import Control.Parallel (parTraverse)
import Data.Array (catMaybes, cons, null)
import Data.Bifunctor (rmap)
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Filterable (filterMap)
import Data.Foldable (foldr)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.UUID (toString)
import Effect.Aff (Aff, catchError)
import Prim.RowList (class RowToList)
import Supabase (Client, FilterBuilder, QueryBuilder, StoragePath(..), Table, download, eq_, from, fromStorage, insert, maybeSingle, mkTable, run, select, update, upload)
import Supabase.Auth.Types (UserId)
import Supabase.Storage (list)
import Supabase.Types (BucketName(..))
import Supabase.UUID (UUID)
import Types (FileContents, FileName, GameId, Image(..), Images, InstructionsKey, Key(..), Instructions)
import Web.File.File (File)
import Web.File.FileReader.Aff as FRA
import Yoga.JSON (class ReadForeignFields)

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

fetchInstructions :: Client -> GameId -> Aff (Array (UserId /\ InstructionsKey /\ Instructions))
fetchInstructions client gameId = do
  results <- client
    # from instructionsTable
    # select
    # eq_ @"bgg_id" (unwrap gameId)
    # run
  pure $ toInstructions <$> fromMaybe [] results.data

fetchSingleInstructions :: Client -> InstructionsKey -> Aff (Maybe (UserId /\ Instructions))
fetchSingleInstructions client key = do
  results <- _.data <$> (client # from instructionsTable # select # eq_ @"instructions_key" (wrap $ unwrap key) # maybeSingle)
  pure $ (\(userId /\ _ /\ ins) -> userId /\ ins) <$> map toInstructions results

fetchImagesForInstructions :: Client -> InstructionsKey -> Aff Images
fetchImagesForInstructions client key = do
  files <- client # fromStorage (BucketName "images") # list (StoragePath (toString $ unwrap key)) {}
  results <- parTraverse (\f -> downloadFile client key f) $ fromMaybe [] files.data
  let filtered = catMaybes results
  pure $ Map.fromFoldable $ map (rmap Downloaded) filtered

downloadFile :: forall r. Client -> InstructionsKey -> { name :: FileName | r } -> Aff (Maybe (Tuple FileName FileContents))
downloadFile client key f = do
  file <- client # fromStorage (BucketName "images") # download (StoragePath $ (toString (unwrap key)) <> "/" <> f.name)
  case file.data of
    Just blob -> do
      Just <<< Tuple f.name <$> FRA.readAsDataURL blob
    Nothing -> pure Nothing

data InstructionsSaveError = FailedToSave String | ImagesFailedToUpload (Array (Tuple FileName String))

saveInstructions
  :: forall t99 t100 t147
   . RowToList t100 t147
  => ReadForeignFields t147 () t100
  => ( { | DbInstructionsForInsertRow }
       -> QueryBuilder DbInstructionsRow ()
       -> FilterBuilder t99 t100
     )
  -> Client
  -> GameId
  -> InstructionsKey
  -> Instructions
  -> Map String Image
  -> Aff (Either InstructionsSaveError Unit)
saveInstructions operation client gameId instructionsKey instructions images = do
  let
    imagesToUpload =
      foldr
        ( \(fileName /\ image) toReturn -> case image of
            Uploaded file _ -> cons (fileName /\ file) toReturn
            _ -> toReturn
        )
        [] $ (Map.toUnfoldable images :: Array (FileName /\ Image))
  results <- client # from instructionsTable # operation (toDbInstructions gameId instructionsKey instructions) # run
  case results.error of
    Nothing -> do
      responses <- traverse (uploadStepImage client instructionsKey) imagesToUpload
      let errors = filterMap (\(k /\ err) -> (k /\ _) <$> err) responses
      if null errors then
        pure $ Right unit
      else
        pure $ Left $ ImagesFailedToUpload errors
    Just err -> pure $ Left $ FailedToSave err.message

newInstructions :: Client -> GameId -> InstructionsKey -> Instructions -> Images -> Aff (Either InstructionsSaveError Unit)
newInstructions = saveInstructions insert

-- TODO: Test this
updateInstructions :: Client -> GameId -> InstructionsKey -> Instructions -> Images -> Aff (Either InstructionsSaveError Unit)
updateInstructions = saveInstructions update

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

toInstructions :: { | DbInstructionsRow } -> UserId /\ InstructionsKey /\ Instructions
toInstructions row = row.created_by /\ Key (unwrap row.instructions_key) /\ row.data