module Message where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson, jsonEmptyObject, (.:), (:=), (~>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import Player (Player)
import Player as Player

newtype JsonPlayer = JsonPlayer Player

derive instance Newtype JsonPlayer _

instance DecodeJson JsonPlayer where
  decodeJson json = decodeJson json >>= case _ of
    "Red" -> pure $ wrap Player.Red
    "Black" -> pure $ wrap Player.Black
    other -> Left $ UnexpectedValue $ encodeJson other

derive instance Generic JsonPlayer _

derive instance Eq JsonPlayer

instance Show JsonPlayer where
  show = genericShow

type GameId = String

type PlayerId = String

type FieldSize = { width :: Int, height :: Int }

type Coordinate = { x :: Int, y :: Int }

type Move = { coordinate :: Coordinate, player :: JsonPlayer }

type OpenGame = { gameId :: GameId, playerId :: PlayerId, size :: FieldSize }

type Game = { gameId :: GameId, size :: FieldSize }

data AuthProvider = GoogleAuthProvider

derive instance Generic AuthProvider _

derive instance Eq AuthProvider

instance EncodeJson AuthProvider where
  encodeJson GoogleAuthProvider = encodeJson "Google"

instance Show AuthProvider where
  show = genericShow

data Request
  = GetAuthUrlRequest AuthProvider
  | AuthRequest String String
  | CreateRequest FieldSize
  | JoinRequest GameId
  | SubscribeRequest GameId
  | UnsubscribeRequest GameId
  | PutPointRequest GameId Coordinate

derive instance Generic Request _

derive instance Eq Request

instance Show Request where
  show = genericShow

instance EncodeJson Request where
  encodeJson (GetAuthUrlRequest provider) = "command" := "GetAuthUrl" ~> "provider" := encodeJson provider ~> jsonEmptyObject
  encodeJson (AuthRequest code state) = "command" := "Auth" ~> "code" := code ~> "state" := state ~> jsonEmptyObject
  encodeJson (CreateRequest size) = "command" := "Create" ~> "size" := size ~> jsonEmptyObject
  encodeJson (JoinRequest gameId) = "command" := "Join" ~> "gameId" := gameId ~> jsonEmptyObject
  encodeJson (SubscribeRequest gameId) = "command" := "Subscribe" ~> "gameId" := gameId ~> jsonEmptyObject
  encodeJson (UnsubscribeRequest gameId) = "command" := "Unsubscribe" ~> "gameId" := gameId ~> jsonEmptyObject
  encodeJson (PutPointRequest gameId coordinate) = "command" := "PutPoint" ~> "gameId" := gameId ~> "coordinate" := coordinate ~> jsonEmptyObject

data Response
  = InitResponse (Array OpenGame) (Array Game)
  | GameInitResponse GameId (Array Move)
  | AuthUrlResponse String
  | AuthResponse PlayerId
  | CreateResponse GameId PlayerId FieldSize
  | StartResponse GameId
  | PutPointResponse GameId Coordinate JsonPlayer

derive instance Generic Response _

derive instance Eq Response

instance Show Response where
  show = genericShow

instance DecodeJson Response where
  decodeJson json = do
    obj <- decodeJson json
    command <- obj .: "command"
    case command of
      "Init" -> InitResponse <$> obj .: "openGames" <*> obj .: "games"
      "GameInit" -> GameInitResponse <$> obj .: "gameId" <*> obj .: "moves"
      "AuthUrl" -> AuthUrlResponse <$> obj .: "url"
      "Auth" -> AuthResponse <$> obj .: "playerId"
      "Create" -> CreateResponse <$> obj .: "gameId" <*> obj .: "playerId" <*> obj .: "size"
      "Start" -> StartResponse <$> obj .: "gameId"
      "PutPoint" -> PutPointResponse <$> obj .: "gameId" <*> obj .: "coordinate" <*> obj .: "player"
      other -> Left $ UnexpectedValue $ encodeJson other
