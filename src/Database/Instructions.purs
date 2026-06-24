module Database.Instructions
  ( fetchInstructions
  , newInstructions
  , fetchSingleInstructions
  , fetchImagesForInstructions
  , updateInstructions
  , fetchUserInstructions
  , InstructionsSaveError(..)
  , deleteInstructions
  ) where

import Prelude

import Control.Parallel (parTraverse)
import Data.Array (catMaybes, cons, filter, null)
import Data.Bifunctor (rmap)
import Data.DateTime (DateTime)
import Data.Either (Either(..), hush)
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
import Foreign (Foreign)
import Prim.RowList (class RowToList)
import Supabase (Client, FilterBuilder, QueryBuilder, StoragePath(..), Table, download, eq_, from, fromStorage, insert, maybeSingle, mkTable, run, select, update, upload, delete)
import Supabase.Auth.Types (UserId)
import Supabase.Storage (list, remove)
import Supabase.Types (BucketName(..))
import Supabase.UUID (UUID)
import Types (FileContents, FileName, FullInstructions, GameId, Image(..), Images, Instructions, InstructionsWithGame, InstructionsWithUser, Key(..), InstructionsKey)
import Web.File.File (File)
import Web.File.FileReader.Aff as FRA
import Yoga.JSON (class ReadForeignFields, read, write)

-- TODO: Clean up the types so I don't have to marshal between them... look at how to leverage row types

type DbInstructionsRow =
  ( id :: Int
  , created_by :: UserId
  , created_at :: DateTime
  | DbInstructionsForInsertRow
  )

type DbInstructionsForInsertRow =
  ( bgg_id :: String
  , data :: Foreign
  , instructions_key :: UUID
  , is_private :: Boolean
  )

instructionsTable :: Table DbInstructionsRow () ()
instructionsTable = mkTable "instructions"

isVisible :: Maybe UserId -> { | DbInstructionsRow } -> Boolean
isVisible userId x = (Just x.created_by == userId && x.is_private) || (not x.is_private)

fetchInstructions :: Client -> Maybe UserId -> GameId -> Aff (Array InstructionsWithUser)
fetchInstructions client userId gameId = do
  -- TODO: We need to patch the original library for this to work properly
  -- let
  --   -- (userId == userId AND private) OR (not private)
  --   privacyFilter = case userId of
  --     Just u -> or (NEA.singleton (eqC @"created_by" u))
  --     Nothing -> ?todo
  results <- client
    # from instructionsTable
    # select
    # eq_ @"bgg_id" (unwrap gameId)
    # run
  -- TODO: Filter on the client, but this isn't ideal (see above)
  pure $ catMaybes $ (map extractData <<< toInstructions) <$> filter (isVisible userId) (fromMaybe [] results.data)
  where
  extractData { createdBy, key, instructions, isPrivate } = { createdBy, key, instructions, isPrivate }

fetchUserInstructions :: Client -> UserId -> Aff (Array InstructionsWithGame)
fetchUserInstructions client userId = do
  results <- client
    # from instructionsTable
    # select
    # eq_ @"created_by" userId
    # run
  pure $ catMaybes $ (map extractData <<< toInstructions) <$> fromMaybe [] results.data
  where
  extractData { gameId, key, instructions, isPrivate } = { gameId, key, instructions, isPrivate }

fetchSingleInstructions :: Client -> Maybe UserId -> InstructionsKey -> Aff (Maybe InstructionsWithUser)
fetchSingleInstructions client userId key = do
  results <- _.data <$>
    ( client
        # from instructionsTable
        # select
        # eq_ @"instructions_key" (wrap $ unwrap key)
        # maybeSingle
    )
  pure $ (\{ createdBy, instructions, isPrivate } -> { createdBy, key, instructions, isPrivate }) <$>
    ( results
        >>= (\x -> if isVisible userId x then Just x else Nothing)
        >>= toInstructions
    )

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
  -> Boolean
  -> InstructionsKey
  -> Instructions
  -> Map String Image
  -> Aff (Either InstructionsSaveError Unit)
saveInstructions operation client gameId isPrivate instructionsKey instructions images = do
  let
    imagesToUpload =
      foldr
        ( \(fileName /\ image) toReturn -> case image of
            Uploaded file _ -> cons (fileName /\ file) toReturn
            _ -> toReturn
        )
        [] $ (Map.toUnfoldable images :: Array (FileName /\ Image))
  results <- client # from instructionsTable # operation (toDbInstructions gameId isPrivate instructionsKey instructions) # run
  case results.error of
    Nothing -> do
      responses <- traverse (uploadStepImage client instructionsKey) imagesToUpload
      let errors = filterMap (\(k /\ err) -> (k /\ _) <$> err) responses
      if null errors then
        pure $ Right unit
      else
        pure $ Left $ ImagesFailedToUpload errors
    Just err -> pure $ Left $ FailedToSave err.message

newInstructions :: Client -> GameId -> Boolean -> InstructionsKey -> Instructions -> Images -> Aff (Either InstructionsSaveError Unit)
newInstructions = saveInstructions insert

-- TODO: Test this
updateInstructions :: Client -> GameId -> Boolean -> InstructionsKey -> Instructions -> Images -> Aff (Either InstructionsSaveError Unit)
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

toDbInstructions :: GameId -> Boolean -> InstructionsKey -> Instructions -> { | DbInstructionsForInsertRow }
toDbInstructions gameId isPrivate instructionsKey instructions =
  { bgg_id: unwrap gameId
  , data: write instructions
  , instructions_key: wrap $ unwrap instructionsKey
  , is_private: isPrivate
  }

toInstructions :: { | DbInstructionsRow } -> Maybe FullInstructions
toInstructions row =
  ( { createdBy: row.created_by
    , gameId: wrap row.bgg_id
    , key: Key (unwrap row.instructions_key)
    , instructions: _
    , isPrivate: row.is_private
    }
  ) <$> deserializeInstructions row.data

deserializeInstructions :: Foreign -> Maybe Instructions
deserializeInstructions obj = hush $ read obj

deleteInstructions :: Client -> InstructionsKey -> Aff (Either String Unit)
deleteInstructions client key = do
  deleteResults <- client
    # from instructionsTable
    # delete
    # eq_ @"instructions_key" (wrap $ unwrap key)
    # run

  case deleteResults.error of
    Just err -> pure $ Left err.message
    Nothing -> do
      -- Need to iterate through images and delete each one
      let storageBucket = fromStorage (BucketName "images") client
      allImages <- storageBucket # list (StoragePath $ toString (unwrap key)) {}
      case allImages.data of
        Just images -> do
          _ <- traverse (\{ name } -> remove [ StoragePath $ (toString (unwrap key)) <> "/" <> name ] storageBucket) images
          pure $ Right unit
        Nothing -> pure $ Right unit
