module Main where

import Prelude

import CSS as CSS
import CSS.Common as CSSCommon
import CSS.Overflow as CSSOverflow
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
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe as Maybe
import Data.Newtype (unwrap, wrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Field as Field
import FieldComponent (fieldComponent, Output(..))
import Foreign (readString)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.Hooks as Hooks
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Message as Message
import Type.Proxy (Proxy(..))
import Web.Event.Event (Event)
import Web.Event.EventTarget as EET
import Web.HTML as HTML
import Web.HTML.Location as Location
import Web.HTML.Window (Window)
import Web.HTML.Window as Window
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS
import Web.URL.URLSearchParams as URLSearchParams

foreign import postMessage :: forall m. Window -> m -> Effect Unit
foreign import eventData :: forall d. Event -> d

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
  :: forall query m
   . MonadAff m
  => H.Component query (Map Message.GameId Message.FieldSize) Message.GameId m
gamesComponent =
  Hooks.component \{ outputToken } games -> Hooks.do
    Hooks.pure $ HH.div_
      $ map (\(Tuple gameId size) -> HH.div [ HE.onClick $ const $ Hooks.raise outputToken gameId ] [ HH.text $ show size.width <> ":" <> show size.height ])
      $ Map.toUnfoldableUnordered games

_openGames :: Proxy "openGames"
_openGames = Proxy

openGamesComponent
  :: forall query m
   . MonadAff m
  => H.Component query (Map Message.GameId Message.FieldSize) Message.GameId m
openGamesComponent =
  Hooks.component \{ outputToken } openGames -> Hooks.do
    Hooks.pure $ HH.div_
      $ map (\(Tuple gameId size) -> HH.div [ HE.onClick $ const $ Hooks.raise outputToken gameId ] [ HH.text $ show size.width <> ":" <> show size.height ])
      $ Map.toUnfoldableUnordered openGames

_createGames :: Proxy "createGames"
_createGames = Proxy

createGameComponent
  :: forall query input m
   . MonadAff m
  => H.Component query input Unit m
createGameComponent =
  Hooks.component \{ outputToken } _ -> Hooks.do
    Hooks.pure $ HH.button [ HE.onClick $ const $ Hooks.raise outputToken unit ] [ HH.text "create game" ]

_login :: Proxy "login"
_login = Proxy

loginComponent
  :: forall query input m
   . MonadAff m
  => H.Component query input Unit m
loginComponent =
  Hooks.component \{ outputToken } _ -> Hooks.do
    Hooks.pure $ HH.button [ HE.onClick $ const $ Hooks.raise outputToken unit ] [ HH.text "login" ]

_field :: Proxy "field"
_field = Proxy

data AppQuery a = AppQuery Message.Response a

appComponent
  :: forall m
   . MonadAff m
  => H.Component AppQuery (Array Message.OpenGame /\ Array Message.Game) Message.Request m
appComponent =
  Hooks.component \{ queryToken, outputToken } (openGamesInput /\ gamesInput) -> Hooks.do
    playerId /\ playerIdId <- Hooks.useState Maybe.Nothing
    openGames /\ openGamesId <- Hooks.useState $ Map.fromFoldable $ map (\{ gameId, size } -> Tuple gameId size) $ openGamesInput
    games /\ gamesId <- Hooks.useState $ Map.fromFoldable $ map (\{ gameId, size } -> Tuple gameId size) gamesInput
    watchingGameId /\ watchingGameIdId <- Hooks.useState Maybe.Nothing
    activeGame /\ activeGameId <- Hooks.useState Maybe.Nothing

    Hooks.useQuery queryToken case _ of
      AppQuery response a -> do
        case response of
          Message.GameInitResponse gameId moves ->
            if Maybe.Just gameId == watchingGameId then
              Hooks.put activeGameId $ Maybe.Just $ gameId /\ Array.foldl
                ( \fields move ->
                    Maybe.maybe fields (_ `NonEmptyList.cons` fields) $ Field.putPoint (Tuple move.coordinate.x move.coordinate.y) (unwrap move.player) $ NonEmptyList.head fields
                )
                (NonEmptyList.singleton $ Field.emptyField 39 32)
                moves
            else
              liftEffect $ Console.warn $ "Unexpected init game id " <> gameId
          Message.AuthUrlResponse url -> liftEffect $ do
            window <- HTML.window
            _ <- Window.open url "_blank" "" window
            pure unit
          Message.AuthResponse playerId' ->
            Hooks.put playerIdId $ Maybe.Just playerId'
          Message.CreateResponse gameId playerId' size ->
            Hooks.modify_ openGamesId $ Map.insert gameId size
          Message.StartResponse gameId -> do
            case Map.lookup gameId openGames of
              Maybe.Nothing -> liftEffect $ Console.warn $ "No open game with id " <> gameId
              Maybe.Just size -> do
                Hooks.modify_ openGamesId $ Map.delete gameId
                Hooks.modify_ gamesId $ Map.insert gameId size
          Message.PutPointResponse gameId coordinate player ->
            case activeGame of
              Maybe.Just (activeGameId' /\ fields) | gameId == activeGameId' ->
                Hooks.put activeGameId
                  $ Maybe.Just
                  $ (/\) activeGameId'
                  $ Maybe.maybe fields (_ `NonEmptyList.cons` fields)
                  $ Field.putPoint (Tuple coordinate.x coordinate.y) (unwrap player)
                  $ NonEmptyList.head fields
              _ ->
                liftEffect $ Console.warn $ "Wrong game to put point"
          _ -> pure unit
        pure $ Maybe.Just a

    Hooks.pure
      $ HH.div
          [ HCSS.style do
              CSS.width $ CSS.pct 100.0
              CSS.height $ CSS.pct 100.0
              CSS.display CSS.grid
              CSS.key (CSS.fromString "grid-template-columns") $ CSS.noCommas [ CSS.rem 10.0, CSSCommon.auto ]
              CSS.key (CSS.fromString "grid-template-rows") $ CSS.noCommas [ CSS.rem 2.0, CSSCommon.auto ]
              CSS.key (CSS.fromString "grid-template-areas") $ CSS.noCommas $ map CSS.quote
                [ "games login"
                , "games field"
                ]
          ]
      $
        [ HH.div
            [ HCSS.style $ do
                CSS.key (CSS.fromString "grid-area") "games"
                CSSOverflow.overflow CSSOverflow.overflowAuto
            ]
            [ HH.slot
                _games
                unit
                gamesComponent
                games
                \gameId -> when (Maybe.Just gameId /= watchingGameId) do
                  Maybe.maybe (pure unit) (\oldGameId -> Hooks.raise outputToken $ Message.UnsubscribeRequest oldGameId) watchingGameId
                  Hooks.put watchingGameIdId $ Maybe.Just gameId
                  Hooks.raise outputToken $ Message.SubscribeRequest gameId
            , HH.slot
                _openGames
                unit
                openGamesComponent
                openGames
                \gameId -> Hooks.raise outputToken $ Message.JoinRequest gameId
            ]
        , HH.div
            [ HCSS.style $ CSS.key (CSS.fromString "grid-area") "field"
            ]
            [ case activeGame of
                Maybe.Just (gameId /\ fields) ->
                  HH.slot
                    _field
                    unit
                    fieldComponent
                    fields
                    \(Click (Tuple x y)) -> Hooks.raise outputToken $ Message.PutPointRequest gameId { x, y }

                Maybe.Nothing ->
                  HH.slot
                    _createGames
                    unit
                    createGameComponent
                    unit
                    \_ -> Hooks.raise outputToken $ Message.CreateRequest { width: 39, height: 32 }
            ]
        , HH.div
            [ HCSS.style $ CSS.key (CSS.fromString "grid-area") "login"
            ]
            [ HH.slot
                _login
                unit
                loginComponent
                openGames
                \_ -> Hooks.raise outputToken $ Message.GetAuthUrlRequest Message.GoogleAuthProvider
            ]
        ]

checkRedirect :: Window -> Effect Boolean
checkRedirect window = do
  maybeOpener <- Window.opener window
  flip (Maybe.maybe (pure false)) maybeOpener \opener -> do
    location <- Window.location window
    url <- Location.search location
    let
      searchParams = URLSearchParams.fromString url
      maybeCode = URLSearchParams.get "code" searchParams
      maybeState = URLSearchParams.get "state" searchParams
    flip (Maybe.maybe (pure false)) (Tuple <$> maybeCode <*> maybeState) \(Tuple code state) -> do
      postMessage opener { code, state }
      Window.close window
      pure true

main :: Effect Unit
main = do
  window <- HTML.window
  redirect <- checkRedirect window
  unless redirect do
    connection <- WS.create "ws://127.0.0.1:8080" []
    listener <- EET.eventListener \event -> do
      let data' = eventData event
      wsSender connection $ Message.AuthRequest data'.code data'.state
    EET.addEventListener (wrap "message") listener true $ Window.toEventTarget window
    HA.runHalogenAff do
      body <- HA.awaitBody
      CR.runProcess $ wsProducer connection CR.$$ do
        openGames /\ games <- CR.await >>= case _ of
          Message.InitResponse openGames games -> pure $ openGames /\ games
          other -> lift $ liftEffect $ Exception.throwException $ Exception.error $ "Unexpected first message: " <> show other
        io <- lift $ runUI appComponent (openGames /\ games) body
        _ <- H.liftEffect $ HS.subscribe io.messages $ wsSender connection
        CR.consumer \response -> (io.query $ H.mkTell $ AppQuery response) *> pure Maybe.Nothing
