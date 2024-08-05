module Message where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, JsonDecodeError(..), decodeJson, encodeJson, jsonEmptyObject, (.:), (.:?), (:=), (~>))
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, wrap)
import Data.Show.Generic (genericShow)
import Player as Player

newtype Color = Color Player.Player

derive instance Newtype Color _

instance DecodeJson Color where
  decodeJson json = decodeJson json >>= case _ of
    "Red" -> pure $ wrap Player.Red
    "Black" -> pure $ wrap Player.Black
    other -> Left $ UnexpectedValue $ encodeJson other

derive instance Generic Color _

derive instance Eq Color

instance Show Color where
  show = genericShow

type GameId = String

type PlayerId = String

type FieldSize = { width :: Int, height :: Int }

type Coordinate = { x :: Int, y :: Int }

type Move = { coordinate :: Coordinate, player :: Color }

type OpenGame = { gameId :: GameId, playerId :: PlayerId, size :: FieldSize }

type Game = { gameId :: GameId, redPlayerId :: PlayerId, blackPlayerId :: PlayerId, size :: FieldSize }

type Player = { playerId :: PlayerId, nickname :: String }

data AuthProvider = GoogleAuthProvider | GitLabAuthProvider | TestAuthProvider

derive instance Generic AuthProvider _

derive instance Eq AuthProvider

instance DecodeJson AuthProvider where
  decodeJson json = decodeJson json >>= case _ of
    "Google" -> Right GoogleAuthProvider
    "GitLab" -> Right GitLabAuthProvider
    "Test" -> Right TestAuthProvider
    other -> Left $ UnexpectedValue $ encodeJson other

instance EncodeJson AuthProvider where
  encodeJson GoogleAuthProvider = encodeJson "Google"
  encodeJson GitLabAuthProvider = encodeJson "GitLab"
  encodeJson TestAuthProvider = encodeJson "Test"

instance Show AuthProvider where
  show = genericShow

data Request
  = GetAuthUrlRequest AuthProvider Boolean
  | AuthRequest String String
  | AuthTestRequest String
  | SignOutRequest
  | CreateRequest FieldSize
  | CloseRequest GameId
  | JoinRequest GameId
  | SubscribeRequest GameId
  | UnsubscribeRequest GameId
  | PutPointRequest GameId Coordinate

derive instance Generic Request _

derive instance Eq Request

instance Show Request where
  show = genericShow

instance EncodeJson Request where
  encodeJson (GetAuthUrlRequest provider rememberMe) = "command" := "GetAuthUrl" ~> "provider" := encodeJson provider ~> "rememberMe" := rememberMe ~> jsonEmptyObject
  encodeJson (AuthRequest code state) = "command" := "Auth" ~> "code" := code ~> "state" := state ~> jsonEmptyObject
  encodeJson (AuthTestRequest name) = "command" := "AuthTest" ~> "name" := name ~> jsonEmptyObject
  encodeJson SignOutRequest = "command" := "SignOut" ~> jsonEmptyObject
  encodeJson (CreateRequest size) = "command" := "Create" ~> "size" := size ~> jsonEmptyObject
  encodeJson (CloseRequest gameId) = "command" := "Close" ~> "gameId" := gameId ~> jsonEmptyObject
  encodeJson (JoinRequest gameId) = "command" := "Join" ~> "gameId" := gameId ~> jsonEmptyObject
  encodeJson (SubscribeRequest gameId) = "command" := "Subscribe" ~> "gameId" := gameId ~> jsonEmptyObject
  encodeJson (UnsubscribeRequest gameId) = "command" := "Unsubscribe" ~> "gameId" := gameId ~> jsonEmptyObject
  encodeJson (PutPointRequest gameId coordinate) = "command" := "PutPoint" ~> "gameId" := gameId ~> "coordinate" := coordinate ~> jsonEmptyObject

data Response
  = InitResponse (Array AuthProvider) (Maybe PlayerId) (Array Player) (Array OpenGame) (Array Game)
  | GameInitResponse GameId (Array Move)
  | AuthUrlResponse String
  | AuthResponse PlayerId String
  | PlayerJoinedResponse Player
  | PlayerLeftResponse PlayerId
  | CreateResponse GameId PlayerId FieldSize
  | CloseResponse GameId
  | StartResponse GameId PlayerId PlayerId
  | PutPointResponse GameId Move

derive instance Generic Response _

derive instance Eq Response

instance Show Response where
  show = genericShow

instance DecodeJson Response where
  decodeJson json = do
    obj <- decodeJson json
    command <- obj .: "command"
    case command of
      "Init" -> InitResponse <$> obj .: "authProviders" <*> obj .:? "playerId" <*> obj .: "players" <*> obj .: "openGames" <*> obj .: "games"
      "GameInit" -> GameInitResponse <$> obj .: "gameId" <*> obj .: "moves"
      "AuthUrl" -> AuthUrlResponse <$> obj .: "url"
      "Auth" -> AuthResponse <$> obj .: "playerId" <*> obj .: "cookie"
      "PlayerJoined" -> PlayerJoinedResponse <$> obj .: "player"
      "PlayerLeft" -> PlayerLeftResponse <$> obj .: "playerId"
      "Create" -> CreateResponse <$> obj .: "gameId" <*> obj .: "playerId" <*> obj .: "size"
      "Close" -> CloseResponse <$> obj .: "gameId"
      "Start" -> StartResponse <$> obj .: "gameId" <*> obj .: "redPlayerId" <*> obj .: "blackPlayerId"
      "PutPoint" -> PutPointResponse <$> obj .: "gameId" <*> obj .: "move"
      other -> Left $ UnexpectedValue $ encodeJson other
