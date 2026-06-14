module Bgg (bggSearch, bggThing, bggThings) where

import Prelude

import Affjax (defaultRequest)
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (document)
import Affjax.Web (printError, request)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Data.Array (catMaybes, head, intercalate)
import Data.Either (Either(..), note)
import Data.HTTP.Method as Http
import Data.Int (fromString)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Traversable (sequence, traverse)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Types (BoardGame, BoardGameSummary, GameId)
import Web.DOM (Document)
import Web.DOM.Document (toParentNode)
import Web.DOM.Element (getAttribute)
import Web.DOM.Element as WDE
import Web.DOM.Node (Node)
import Web.DOM.Node as Node
import Web.DOM.NodeList (toArray)
import Web.DOM.ParentNode (QuerySelector(..), querySelector, querySelectorAll)

bggSearch :: String -> Aff (Either String (Array BoardGameSummary))
bggSearch search = do
  let
    req = defaultRequest
      { url = searchUrl search
      , method = Left Http.GET
      , headers = [ RequestHeader "Authorization" ("Bearer " <> bggKey) ]
      , responseFormat = document
      }
  resp <- request req
  case resp of
    Left err -> pure $ Left (printError err)
    Right body -> note "There was a problem reading the XML" <$> liftEffect (searchParser body.body)

bggThing :: GameId -> Aff (Either String BoardGame)
bggThing gameId = do
  let
    req = defaultRequest
      { url = thingUrl [ gameId ]
      , method = Left Http.GET
      , headers = [ RequestHeader "Authorization" ("Bearer " <> bggKey) ]
      , responseFormat = document
      }
  resp <- request req
  case resp of
    Left err -> pure $ Left (printError err)
    Right body -> note "There was a problem reading the XML" <$> liftEffect (head <$> thingParser body.body)

bggThings :: Array GameId -> Aff (Either String (Array BoardGame))
bggThings gameIds = do
  let
    req = defaultRequest
      { url = thingUrl gameIds
      , method = Left Http.GET
      , headers = [ RequestHeader "Authorization" ("Bearer " <> bggKey) ]
      , responseFormat = document
      }
  resp <- request req
  case resp of
    Left err -> pure $ Left (printError err)
    Right body -> Right <$> liftEffect (thingParser body.body)

bggKey :: String
bggKey = "e6930218-35cd-4fd8-830f-05c987bae61b"

searchUrl :: String -> String
searchUrl search = "https://boardgamegeek.com/xmlapi2/search?type=boardgame&query=" <> search

thingUrl :: Array GameId -> String
thingUrl gameIds = "https://boardgamegeek.com/xmlapi2/thing?type=boardgame&id=" <> intercalate "," (unwrap <$> gameIds)

searchParser :: Document -> Effect (Maybe (Array BoardGameSummary))
searchParser doc = do
  let parentNode = toParentNode doc
  allItems <- querySelectorAll (QuerySelector "item") parentNode >>= toArray
  sequence <$> traverse parseSearchNode allItems

parseSearchNode :: Node -> Effect (Maybe BoardGameSummary)
parseSearchNode node = do
  case WDE.fromNode node of
    Just element -> do
      let parentNode = WDE.toParentNode element
      results <- runMaybeT do
        bggId <- MaybeT $ WDE.getAttribute "id" element
        nameElement <- MaybeT $ querySelector (QuerySelector "name") parentNode
        title <- MaybeT $ WDE.getAttribute "value" nameElement
        pure
          { bggId: wrap bggId
          , title
          }
      yearPublished <- runMaybeT do
        yearPublishedElement <- MaybeT $ querySelector (QuerySelector "yearpublished") parentNode
        yearPublishedValue <- MaybeT $ WDE.getAttribute "value" yearPublishedElement
        MaybeT $ pure $ fromString yearPublishedValue
      pure $ (\{ bggId, title } -> { bggId, title, yearPublished }) <$> results

    Nothing -> pure Nothing

thingParser :: Document -> Effect (Array BoardGame)
thingParser doc = do
  let parentNode = toParentNode doc
  items <- querySelectorAll (QuerySelector "item") parentNode >>= toArray
  results <- traverse parseThing (catMaybes $ WDE.fromNode <$> items)
  pure $ catMaybes results

parseThing :: WDE.Element -> Effect (Maybe BoardGame)
parseThing element = do
  runMaybeT do
    let parentNode = WDE.toParentNode element
    bggId <- MaybeT $ getAttribute "id" element

    thumbnailUrlElement <- MaybeT $ querySelector (QuerySelector "thumbnail") parentNode
    thumbnailUrl <- MaybeT $ map Just $ Node.textContent (WDE.toNode thumbnailUrlElement)

    imageUrlElement <- MaybeT $ querySelector (QuerySelector "image") parentNode
    imageUrl <- MaybeT $ map Just $ Node.textContent (WDE.toNode imageUrlElement)

    titleElement <- MaybeT $ querySelector (QuerySelector "name[type='primary']") parentNode
    title <- MaybeT $ getAttribute "value" titleElement

    yearPublishedElement <- MaybeT $ querySelector (QuerySelector "yearpublished") parentNode
    yearPublished <- MaybeT $ map Int.fromString <$> getAttribute "value" yearPublishedElement

    expansionNodes <- MaybeT $ Just <$> (querySelectorAll (QuerySelector "link[type='boardgameexpansion']") parentNode >>= toArray)

    expansions <- MaybeT $ map Just $ catMaybes <$> traverse parseExpansion expansionNodes

    pure { bggId: wrap bggId, thumbnailUrl, imageUrl, title, yearPublished, expansions }

parseExpansion :: Node -> Effect (Maybe { gameId :: GameId, title :: String })
parseExpansion node =
  case WDE.fromNode node of
    Just element -> runMaybeT do
      id <- MaybeT $ getAttribute "id" element
      title <- MaybeT $ getAttribute "value" element
      pure { gameId: wrap id, title: title }
    Nothing -> pure Nothing