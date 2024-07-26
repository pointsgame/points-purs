module Message where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson, (.:), (:=), (~>))
import Data.Either (Either(..))

type GameId = String

type FieldSize = { width :: Int, height :: Int }

type Coordinate = { x :: Int, y :: Int }

type OpenGame = { gameId :: GameId, size :: FieldSize }

type Game = { gameId :: GameId, size :: FieldSize }

data Request
  = CreateRequest FieldSize
  | JoinRequest GameId
  | SubscribeRequest GameId
  | UnsubscribeRequest GameId
  | PutPointRequest GameId Coordinate

instance EncodeJson Request where
  encodeJson (CreateRequest size) = "command" := "Create" ~> "size" := size
  encodeJson (JoinRequest gameId) = "command" := "Join" ~> "gameId" := gameId
  encodeJson (SubscribeRequest gameId) = "command" := "Subscribe" ~> "gameId" := gameId
  encodeJson (UnsubscribeRequest gameId) = "command" := "Unsubscribe" ~> "gameId" := gameId
  encodeJson (PutPointRequest gameId coordinate) = "command" := "PutPoint" ~> "gameId" := gameId ~> "coordinate" := coordinate

data Response
  = InitResponse (Array OpenGame) (Array Game)
  | GameInitResponse
  | CreateResponse GameId FieldSize
  | StartResponse GameId
  | PutPointResponse GameId Coordinate

instance DecodeJson Response where
  decodeJson json = do
    obj <- decodeJson json
    command <- obj .: "command"
    case command of
      "Init" -> InitResponse <$> obj .: "openGames" <*> obj .: "games"
      "GameInit" -> pure $ GameInitResponse
      "Create" -> CreateResponse <$> obj .: "gameId" <*> obj .: "size"
      "Start" -> StartResponse <$> obj .: "gameId"
      "PutPoint" -> PutPointResponse <$> obj .: "gameId" <*> obj .: "coordinate"
      other -> Left $ UnexpectedValue $ encodeJson other
