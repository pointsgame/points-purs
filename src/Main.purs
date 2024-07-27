module Main where

import Prelude

import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (decodeJson, encodeJson, parseJson, stringify)
import Data.Array as Array
import Data.Either as Either
import Data.Foldable (for_)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe as Maybe
import Data.Tuple.Nested ((/\), type (/\))
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
import Halogen.HTML.Events as HE
import Halogen.Subscription as HS
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

_games :: Proxy "games"
_games = Proxy

gamesComponent
  :: forall output m
   . MonadAff m
  => H.Component AppQuery (Array Message.Game) output m
gamesComponent =
  Hooks.component \_ games -> Hooks.do
    Hooks.pure $ HH.div_ $
      map (\game -> HH.div_ $ [ HH.text game.gameId ]) games

_openGames :: Proxy "openGames"
_openGames = Proxy

openGamesComponent
  :: forall output m
   . MonadAff m
  => H.Component AppQuery (Array Message.OpenGame) output m
openGamesComponent =
  Hooks.component \_ openGames -> Hooks.do
    Hooks.pure $ HH.div_ $
      map (\openGame -> HH.div_ $ [ HH.text openGame.gameId ]) openGames

_createGames :: Proxy "createGames"
_createGames = Proxy

createGameComponent
  :: forall query input m
   . MonadAff m
  => H.Component query input Unit m
createGameComponent =
  Hooks.component \{ outputToken } _ -> Hooks.do
    Hooks.pure $ HH.button [ HE.onClick \_ -> Hooks.raise outputToken unit ] [ HH.text "create game" ]

data AppQuery a = AppQuery Message.Response a

appComponent
  :: forall m
   . MonadAff m
  => H.Component AppQuery (Array Message.OpenGame /\ Array Message.Game) Message.Request m
appComponent =
  Hooks.component \{ queryToken, outputToken } (openGames' /\ games') -> Hooks.do
    openGames /\ openGamesId <- Hooks.useState openGames'
    games /\ gamesId <- Hooks.useState games'

    Hooks.useQuery queryToken case _ of
      AppQuery response a -> do
        case response of
          Message.CreateResponse gameId size ->
            Hooks.modify_ openGamesId $ Array.cons { gameId, size }
          _ -> pure unit
        pure $ Maybe.Just a

    Hooks.pure $ HH.div_
      [ HH.slot_
          _games
          unit
          gamesComponent
          games
      , HH.slot_
          _openGames
          unit
          openGamesComponent
          openGames
      , HH.slot
          _createGames
          unit
          createGameComponent
          unit
          \_ -> Hooks.raise outputToken $ Message.CreateRequest { width: 39, height: 32 }
      ]

-- appComponent =
--   Hooks.component \_ _ -> Hooks.do
--     fields /\ fieldsId <- Hooks.useState $ NonEmptyList.singleton $ Field.emptyField 10 10

--     let
--       handleFieldOutput (Click pos) = Hooks.modify_ fieldsId $ \fields' ->
--         Maybe.maybe fields' (_ `NonEmptyList.cons` fields') $ Field.putNextPoint pos $ NonEmptyList.head fields'

--     Hooks.pure $ HH.slot
--       _field
--       unit
--       fieldComponent
--       fields
--       handleFieldOutput

-- _field :: Proxy "field"
-- _field = Proxy

main :: Effect Unit
main = do
  connection <- WS.create "ws://127.0.0.1:8080" []
  HA.runHalogenAff do
    body <- HA.awaitBody
    CR.runProcess $ wsProducer connection CR.$$ do
      openGames /\ games <- CR.await >>= case _ of
        Message.InitResponse openGames games -> pure $ openGames /\ games
        other -> lift $ liftEffect $ Exception.throwException $ Exception.error $ "Unexpected first message: " <> show other
      io <- lift $ runUI appComponent (openGames /\ games) body
      _ <- H.liftEffect $ HS.subscribe io.messages $ wsSender connection
      CR.consumer \response -> (io.query $ H.mkTell $ AppQuery response) *> pure Maybe.Nothing
