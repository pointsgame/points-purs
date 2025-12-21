module Message where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, JsonDecodeError(..), caseJsonObject, decodeJson, encodeJson, jsonEmptyObject, (.:), (.:?), (:=), (~>))
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)
import Data.Traversable (traverse)
import Data.Tuple (Tuple)
import Foreign.Object as Object
import Player as Player

newtype JsonInstant = JsonInstant Instant

derive instance Newtype JsonInstant _

instance DecodeJson JsonInstant where
  decodeJson json = decodeJson json >>= (map wrap <<< Maybe.maybe (Left $ UnexpectedValue json) pure <<< Instant.instant <<< wrap)

newtype StringMap a = StringMap (Map String a)

instance DecodeJson a => DecodeJson (StringMap a) where
  decodeJson json =
    caseJsonObject (Left $ UnexpectedValue json)
      ( \obj ->
          map StringMap $ traverse decodeJson (Map.fromFoldable (Object.toUnfoldable obj :: Array (Tuple String Json)))
      )
      json

unwrapStringMap :: forall a. StringMap a -> Map String a
unwrapStringMap (StringMap map) = map

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

type GameTime = { total :: Int, increment :: Int }

type TimeLeft = { red :: Int, black :: Int }

type GameConfig = { size :: FieldSize, time :: GameTime }

type Coordinate = { x :: Int, y :: Int }

type Move = { coordinate :: Coordinate, player :: Color }

type Player = { nickname :: String }

type OpenGame = { playerId :: PlayerId, player :: Player, config :: GameConfig }

type Game = { redPlayerId :: PlayerId, blackPlayerId :: PlayerId, redPlayer :: Player, blackPlayer :: Player, config :: GameConfig }

type Players = Map PlayerId Player
type OpenGames = Map GameId OpenGame
type Games = Map GameId Game

data DrawReason = AgreementDrawReason | GroundedDrawReason

derive instance Generic DrawReason _

derive instance Eq DrawReason

instance DecodeJson DrawReason where
  decodeJson json = decodeJson json >>= case _ of
    "Agreement" -> Right AgreementDrawReason
    "Grounded" -> Right GroundedDrawReason
    other -> Left $ UnexpectedValue $ encodeJson other

instance Show DrawReason where
  show = genericShow

data WinReason = ResignedWinReason | GroundedWinReason | TimeOutWinReason

derive instance Generic WinReason _

derive instance Eq WinReason

instance DecodeJson WinReason where
  decodeJson json = decodeJson json >>= case _ of
    "Resigned" -> Right ResignedWinReason
    "Grounded" -> Right GroundedWinReason
    "TimeOut" -> Right TimeOutWinReason
    other -> Left $ UnexpectedValue $ encodeJson other

instance Show WinReason where
  show = genericShow

data GameResult
  = WinGameResult Color WinReason
  | DrawGameResult DrawReason

instance DecodeJson GameResult where
  decodeJson json = do
    obj <- decodeJson json
    t <- obj .: "type"
    case t of
      "Win" -> WinGameResult <$> obj .: "winner" <*> obj .: "reason"
      "Draw" -> DrawGameResult <$> obj .: "reason"
      other -> Left $ UnexpectedValue $ encodeJson other

derive instance Generic GameResult _

derive instance Eq GameResult

instance Show GameResult where
  show = genericShow

data Request
  = GetAuthUrlRequest Boolean
  | AuthRequest String String
  | AuthTestRequest String
  | SignOutRequest
  | CreateRequest GameConfig
  | CloseRequest GameId
  | JoinRequest GameId
  | SubscribeRequest GameId
  | UnsubscribeRequest GameId
  | PutPointRequest GameId Coordinate
  | ResignRequest GameId
  | DrawRequest GameId

derive instance Generic Request _

derive instance Eq Request

instance Show Request where
  show = genericShow

instance EncodeJson Request where
  encodeJson (GetAuthUrlRequest rememberMe) = "command" := "GetAuthUrl" ~> "rememberMe" := rememberMe ~> jsonEmptyObject
  encodeJson (AuthRequest code state) = "command" := "Auth" ~> "code" := code ~> "state" := state ~> jsonEmptyObject
  encodeJson (AuthTestRequest name) = "command" := "AuthTest" ~> "name" := name ~> jsonEmptyObject
  encodeJson SignOutRequest = "command" := "SignOut" ~> jsonEmptyObject
  encodeJson (CreateRequest config) = "command" := "Create" ~> "config" := config ~> jsonEmptyObject
  encodeJson (CloseRequest gameId) = "command" := "Close" ~> "gameId" := gameId ~> jsonEmptyObject
  encodeJson (JoinRequest gameId) = "command" := "Join" ~> "gameId" := gameId ~> jsonEmptyObject
  encodeJson (SubscribeRequest gameId) = "command" := "Subscribe" ~> "gameId" := gameId ~> jsonEmptyObject
  encodeJson (UnsubscribeRequest gameId) = "command" := "Unsubscribe" ~> "gameId" := gameId ~> jsonEmptyObject
  encodeJson (PutPointRequest gameId coordinate) = "command" := "PutPoint" ~> "gameId" := gameId ~> "coordinate" := coordinate ~> jsonEmptyObject
  encodeJson (ResignRequest gameId) = "command" := "Resign" ~> "gameId" := gameId ~> jsonEmptyObject
  encodeJson (DrawRequest gameId) = "command" := "Draw" ~> "gameId" := gameId ~> jsonEmptyObject

data Response
  = InitResponse (Maybe PlayerId) (Map PlayerId Player) (Map GameId OpenGame) (Map GameId Game)
  | GameInitResponse GameId Game (Array Move) Instant (Maybe Color) TimeLeft
  | AuthUrlResponse String
  | AuthResponse PlayerId String
  | PlayerJoinedResponse PlayerId Player
  | PlayerLeftResponse PlayerId
  | CreateResponse GameId OpenGame
  | CloseResponse GameId
  | StartResponse GameId Game
  | PutPointResponse GameId Move Instant TimeLeft
  | DrawResponse GameId Color
  | GameResultResponse GameId GameResult
  | NicknameChanged PlayerId Player

derive instance Generic Response _

derive instance Eq Response

instance Show Response where
  show = genericShow

instance DecodeJson Response where
  decodeJson json = do
    let
      un :: JsonInstant -> Instant
      un = unwrap
    obj <- decodeJson json
    command <- obj .: "command"
    case command of
      "Init" -> InitResponse
        <$> obj .:? "playerId"
        <*> (map unwrapStringMap $ obj .: "players")
        <*> (map unwrapStringMap $ obj .: "openGames")
        <*> (map unwrapStringMap $ obj .: "games")
      "GameInit" -> GameInitResponse
        <$> obj .: "gameId"
        <*> obj .: "game"
        <*> obj .: "moves"
        <*> (map un $ obj .: "initTime")
        <*> obj .:? "drawOffer"
        <*> obj .: "timeLeft"
      "AuthUrl" -> AuthUrlResponse <$> obj .: "url"
      "Auth" -> AuthResponse <$> obj .: "playerId" <*> obj .: "cookie"
      "PlayerJoined" -> PlayerJoinedResponse <$> obj .: "playerId" <*> obj .: "player"
      "PlayerLeft" -> PlayerLeftResponse <$> obj .: "playerId"
      "Create" -> CreateResponse <$> obj .: "gameId" <*> obj .: "openGame"
      "Close" -> CloseResponse <$> obj .: "gameId"
      "Start" -> StartResponse <$> obj .: "gameId" <*> obj .: "game"
      "PutPoint" -> PutPointResponse <$> obj .: "gameId" <*> obj .: "move" <*> (map un $ obj .: "puttingTime") <*> obj .: "timeLeft"
      "Draw" -> DrawResponse <$> obj .: "gameId" <*> obj .: "player"
      "GameResult" -> GameResultResponse <$> obj .: "gameId" <*> obj .: "gameResult"
      "NicknameChanged" -> NicknameChanged <$> obj .: "playerId" <*> obj .: "player"
      other -> Left $ UnexpectedValue $ encodeJson other
