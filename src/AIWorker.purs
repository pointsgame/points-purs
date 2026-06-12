-- | Bindings to the OpPAI web worker from `oppai-rs` which communicates with
-- | JSON messages as defined by the `oppai-protocol` crate.
module AIWorker
  ( AIWorker
  , Coords
  , Request(..)
  , Response(..)
  , create
  , post
  , terminate
  ) where

import Prelude

import Data.Argonaut (JsonDecodeError(..), decodeJson, encodeJson, jsonEmptyObject, parseJson, stringify, (.:), (:=), (~>))
import Data.Either (Either(..))
import Effect (Effect)
import Player as Player

foreign import data AIWorker :: Type

foreign import _create :: (String -> Effect Unit) -> Effect AIWorker
foreign import _post :: AIWorker -> String -> Effect Unit
foreign import terminate :: AIWorker -> Effect Unit

type Coords = { x :: Int, y :: Int }

data Request
  = Init { width :: Int, height :: Int }
  | PutPoint Coords Player.Player
  | Undo
  -- | Request the best moves for a player within a time limit in milliseconds.
  | Analyze Player.Player Int

data Response
  = InitResponse
  | PutPointResponse Boolean
  | UndoResponse Boolean
  | AnalyzeResponse (Array { coords :: Coords, weight :: Number })

encodePlayer :: Player.Player -> String
encodePlayer Player.Red = "Red"
encodePlayer Player.Black = "Black"

encodeRequest :: Request -> String
encodeRequest (Init size) = stringify
  $ "command" := "Init"
      ~> "width" := size.width
      ~> "height" := size.height
      ~> jsonEmptyObject
encodeRequest (PutPoint coords player) = stringify
  $ "command" := "PutPoint"
      ~> "coords" := encodeJson coords
      ~> "player" := encodePlayer player
      ~> jsonEmptyObject
encodeRequest Undo = stringify
  $ "command" := "Undo"
      ~> jsonEmptyObject
encodeRequest (Analyze player time) = stringify
  $ "command" := "Analyze"
      ~> "player" := encodePlayer player
      ~> "constraint" := ("type" := "Time" ~> "value" := time ~> jsonEmptyObject)
      ~> jsonEmptyObject

decodeResponse :: String -> Either JsonDecodeError Response
decodeResponse message = do
  obj <- parseJson message >>= decodeJson
  command <- obj .: "command"
  case command of
    "Init" -> pure InitResponse
    "PutPoint" -> PutPointResponse <$> obj .: "put"
    "Undo" -> UndoResponse <$> obj .: "undone"
    "Analyze" -> AnalyzeResponse <$> obj .: "moves"
    _ -> Left $ TypeMismatch $ "unknown command " <> command

create :: (Either JsonDecodeError Response -> Effect Unit) -> Effect AIWorker
create onMessage = _create $ onMessage <<< decodeResponse

post :: AIWorker -> Request -> Effect Unit
post worker = _post worker <<< encodeRequest
