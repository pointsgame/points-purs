module Main where

import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (decodeJson, encodeJson, parseJson, stringify)
import Data.Either as Either
import Data.Foldable (for_)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe as Maybe
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Exception as Exception
import Field as Field
import FieldComponent (fieldComponent, Output(..))
import Foreign (readString)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)
import Message as Message
import Type.Proxy (Proxy(..))
import Web.Event.EventTarget as EET
import Web.HTML as HTML
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS

refresh :: Effect Unit
refresh = HTML.window >>= Window.location >>= Location.reload

-- TODO: reconnect: https://stackoverflow.com/questions/22431751/websocket-how-to-automatically-reconnect-after-it-dies
-- TODO: refresh on error: https://stackoverflow.com/questions/14787480/page-refresh-in-case-of-javascript-errors
wsProducer :: WS.WebSocket -> CR.Producer Message.Response Aff Unit
wsProducer socket = CRA.produce \emitter -> do
  listener <- EET.eventListener \ev ->
    for_ (ME.fromEvent ev) \msgEvent ->
      for_ (Either.either (const Maybe.Nothing) Maybe.Just $ runExcept $ readString $ ME.data_ msgEvent) \msg -> do
        response <- Either.either
          (\e -> liftEffect $ Exception.throwException $ Exception.error $ "Invalid response: " <> show e)
          pure
          (parseJson msg >>= decodeJson)
        emit emitter response
  EET.addEventListener
    WSET.onMessage
    listener
    false
    (WS.toEventTarget socket)

wsSender :: WS.WebSocket -> Message.Request -> Effect Unit
wsSender socket = WS.sendString socket <<< stringify <<< encodeJson

_field :: Proxy "field"
_field = Proxy

data AppQuery a = AppQuery Message.Response a

appComponent
  :: forall input output m
   . MonadAff m
  => H.Component AppQuery input output m
appComponent =
  Hooks.component \_ _ -> Hooks.do
    fields /\ fieldsId <- Hooks.useState $ NonEmptyList.singleton $ Field.emptyField 10 10

    let
      handleFieldOutput (Click pos) = Hooks.modify_ fieldsId $ \fields' ->
        Maybe.maybe fields' (_ `NonEmptyList.cons` fields') $ Field.putNextPoint pos $ NonEmptyList.head fields'

    Hooks.pure $ HH.slot
      _field
      unit
      fieldComponent
      fields
      handleFieldOutput

main :: Effect Unit
main = do
  connection <- WS.create "ws://127.0.0.1:8080" []
  HA.runHalogenAff do
    body <- HA.awaitBody
    CR.runProcess $ wsProducer connection CR.$$ do
      openGames /\ games <- CR.await >>= case _ of
        Message.InitResponse openGames games -> pure $ openGames /\ games
        other -> lift $ liftEffect $ Exception.throwException $ Exception.error $ "Unexpected first message: " <> show other
      io <- lift $ runUI appComponent unit body
      CR.consumer \response -> (io.query $ H.mkTell $ AppQuery response) *> pure Maybe.Nothing
