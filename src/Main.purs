module Main where

import Prelude

import CSS as CSS
import CSS.Common as CSSCommon
import CSS.Cursor as CSSCursor
import CSS.Overflow as CSSOverflow
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (decodeJson, encodeJson, parseJson, stringify)
import Data.Array as Array
import Data.Either as Either
import Data.Foldable (for_, traverse_)
import Data.List.NonEmpty as NonEmptyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Newtype (unwrap, wrap)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Field as Field
import FieldComponent (fieldComponent, Output(..), Redraw(..))
import Foreign (readString)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Subscription as HS
import Halogen.VDom.Driver (runUI)
import Message as Message
import Type.Proxy (Proxy(..))
import Web.DOM.Element as Element
import Web.DOM.MutationObserver as MutationObserver
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (Event)
import Web.Event.EventTarget as EET
import Web.HTML as HTML
import Web.HTML.HTMLDocument as Document
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.Location as Location
import Web.HTML.Window (Window)
import Web.HTML.Window as Window
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS
import Web.URL.URLSearchParams as URLSearchParams

foreign import postMessage :: forall m. Window -> m -> Effect Unit
foreign import eventData :: forall d. Event -> d

fr :: Number -> CSS.Size CSS.LengthUnit
fr i = CSS.BasicSize (CSS.value i <> CSS.fromString "fr")

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

type Game = { redPlayerId :: Message.PlayerId, blackPlayerId :: Message.PlayerId, size :: Message.FieldSize }
type OpenGame = { playerId :: Message.PlayerId, size :: Message.FieldSize }

_games :: Proxy "games"
_games = Proxy

gamesComponent
  :: forall query m
   . MonadAff m
  => H.Component query (Maybe Message.PlayerId /\ Map Message.GameId Game) Message.GameId m
gamesComponent =
  Hooks.component \{ outputToken } (activePlayerId /\ games) -> Hooks.do
    Hooks.pure $ HH.div_
      $ map
          ( \(Tuple gameId { size }) -> HH.div
              [ HE.onClick $ const $ Hooks.raise outputToken gameId ]
              [ HH.text $ show size.width <> ":" <> show size.height ]
          )
      $ Map.toUnfoldableUnordered games

_openGames :: Proxy "openGames"
_openGames = Proxy

openGamesComponent
  :: forall query m
   . MonadAff m
  => H.Component query (Maybe Message.PlayerId /\ Map Message.GameId OpenGame) Message.GameId m
openGamesComponent =
  Hooks.component \{ outputToken } (activePlayerId /\ openGames) -> Hooks.do
    Hooks.pure $ HH.div_
      $ map
          ( \(Tuple gameId { size }) -> HH.div
              [ HE.onClick $ const $ when (Maybe.isJust activePlayerId && map _.playerId (Map.lookup gameId openGames) /= activePlayerId) $
                  Hooks.raise outputToken gameId
              ]
              [ HH.text $ show size.width <> ":" <> show size.height ]
          )
      $ Map.toUnfoldableUnordered openGames

_createGame :: Proxy "createGame"
_createGame = Proxy

data CreateGameOutput = CreateGame Int Int | CloseGame Message.GameId

createGameComponent
  :: forall query m
   . MonadAff m
  => H.Component query (Maybe Message.PlayerId /\ Map Message.GameId OpenGame) CreateGameOutput m
createGameComponent =
  Hooks.component \{ outputToken } (activePlayerId /\ openGames) -> Hooks.do
    Hooks.pure $
      if Maybe.isJust activePlayerId then
        case Array.find (\(Tuple _ { playerId }) -> Maybe.Just playerId == activePlayerId) $ Map.toUnfoldable openGames of
          Maybe.Nothing ->
            HH.button
              [ HE.onClick $ const $ Hooks.raise outputToken $ CreateGame 39 32 ]
              [ HH.text "Create game" ]
          Maybe.Just (Tuple gameId _) ->
            HH.button
              [ HE.onClick $ const $ Hooks.raise outputToken $ CloseGame gameId ]
              [ HH.text "Cancel game" ]
      else
        HH.text "Sign in to play!"

_signin :: Proxy "signin"
_signin = Proxy

buttonStyle :: CSS.CSS
buttonStyle = do
  traverse_ CSS.color $ CSS.fromHexString "#333"
  CSS.key (CSS.fromString "border") "none"
  CSS.padding (CSS.px 5.0) (CSS.px 10.0) (CSS.px 5.0) (CSS.px 10.0)
  CSS.borderRadius (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0)
  CSS.cursor CSSCursor.pointer

data SignInOutput = SignIn Message.AuthProvider | SignInTest String --- | SignOut

signinComponent
  :: forall query input m
   . MonadAff m
  => H.Component query input SignInOutput m
signinComponent =
  Hooks.component \{ outputToken } _ -> Hooks.do
    Hooks.pure $ HH.div
      [ HP.id "sign-in"
      , HCSS.style $ CSS.position $ CSS.relative
      ]
      [ HH.button
          [ HP.id "sign-in-btn"
          , HCSS.style buttonStyle
          ]
          [ HH.text "Sign in" ]
      , HH.div
          [ HP.id "sign-in-list"
          , HCSS.style do
              CSS.position CSS.absolute
              CSS.top $ CSS.pct 100.0
              CSS.right $ CSS.px 0.0
              traverse_ CSS.backgroundColor $ CSS.fromHexString "#fff"
              traverse_ (CSS.border CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#ddd"
              CSS.padding (CSS.px 10.0) (CSS.px 10.0) (CSS.px 10.0) (CSS.px 10.0)
              CSS.zIndex 1
          ]
          [ HH.div
              [ HCSS.style do
                  traverse_ (CSS.borderBottom CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#ddd"
              ]
              [ HH.button
                  [ HP.class_ $ wrap "sign-in-provider"
                  , HCSS.style buttonStyle
                  , HE.onClick $ const $ Hooks.raise outputToken $ SignIn Message.GoogleAuthProvider
                  ]
                  [ HH.text "Google" ]
              ]
          , HH.div
              [ HCSS.style $ CSS.display CSS.flex
              ]
              [ HH.button
                  [ HP.class_ $ wrap "sign-in-provider"
                  , HCSS.style buttonStyle
                  , HE.onClick $ const $ do
                      maybeName <- liftEffect do
                        window <- HTML.window
                        document <- Window.document window
                        maybeInput <- map (_ >>= HTMLInputElement.fromElement) $ getElementById "test-name" (Document.toNonElementParentNode document)
                        traverse HTMLInputElement.value maybeInput
                      for_ maybeName \name -> do
                        Hooks.raise outputToken $ SignInTest name
                  ]
                  [ HH.text "Test" ]
              , HH.input
                  [ HP.id "test-name"
                  , HCSS.style do
                      CSS.width $ CSS.rem 4.0
                      CSS.margin (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0)
                  ]
              ]
          ]
      ]

_field :: Proxy "field"
_field = Proxy

data AppQuery a = AppQuery Message.Response a

appComponent
  :: forall m
   . MonadAff m
  => H.Component AppQuery (Array Message.OpenGame /\ Array Message.Game) Message.Request m
appComponent =
  Hooks.component \{ queryToken, outputToken, slotToken } (openGamesInput /\ gamesInput) -> Hooks.do
    activePlayerId /\ activePlayerIdId <- Hooks.useState Maybe.Nothing
    openGames /\ openGamesId <- Hooks.useState $ Map.fromFoldable $ map (\{ gameId, playerId, size } -> Tuple gameId { playerId, size }) $ openGamesInput
    games /\ gamesId <- Hooks.useState $ Map.fromFoldable $ map (\{ gameId, redPlayerId, blackPlayerId, size } -> Tuple gameId { redPlayerId, blackPlayerId, size }) gamesInput
    watchingGameId /\ watchingGameIdId <- Hooks.useState Maybe.Nothing
    activeGame /\ activeGameId <- Hooks.useState Maybe.Nothing

    Hooks.useLifecycleEffect do
      let
        emitter = HS.makeEmitter \push -> do
          observer <- MutationObserver.mutationObserver \_ _ -> push $ Hooks.tell slotToken _field unit Redraw
          window <- HTML.window
          document <- Window.document window
          void $ runMaybeT $ do
            sidePanel <- wrap $ getElementById "side-panel" $ Document.toNonElementParentNode document
            lift $ MutationObserver.observe (Element.toNode sidePanel) { attributes: true } observer
          pure $ MutationObserver.disconnect observer
      subscriptionId <- Hooks.subscribe emitter
      pure $ Maybe.Just $ Hooks.unsubscribe subscriptionId

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
          Message.AuthResponse playerId ->
            Hooks.put activePlayerIdId $ Maybe.Just playerId
          Message.CreateResponse gameId playerId size ->
            Hooks.modify_ openGamesId $ Map.insert gameId { playerId, size }
          Message.CloseResponse gameId ->
            Hooks.modify_ openGamesId $ Map.delete gameId
          Message.StartResponse gameId redPlayerId blackPlayerId -> do
            case Map.lookup gameId openGames of
              Maybe.Nothing -> liftEffect $ Console.warn $ "No open game with id " <> gameId
              Maybe.Just { size } -> do
                Hooks.modify_ openGamesId $ Map.delete gameId
                Hooks.modify_ gamesId $ Map.insert gameId { redPlayerId, blackPlayerId, size }
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
              CSS.display CSS.flex
              CSS.flexDirection CSS.column
          ]
      $
        [ HH.div
            [ HCSS.style do
                CSS.display CSS.flex
                CSS.justifyContent CSS.spaceBetween
                CSS.alignItems CSSCommon.center
                CSS.padding (CSS.rem 0.5) (CSS.rem 0.5) (CSS.rem 0.5) (CSS.rem 0.5)
                traverse_ CSS.backgroundColor $ CSS.fromHexString "#f2f2f2"
                traverse_ (CSS.borderBottom CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#ddd"
            ]
            [ HH.div
                [ HCSS.style $ CSS.marginLeft CSSCommon.auto
                ]
                [ HH.slot
                    _signin
                    unit
                    signinComponent
                    openGames
                    case _ of
                      SignIn provider -> Hooks.raise outputToken $ Message.GetAuthUrlRequest provider
                      SignInTest name -> Hooks.raise outputToken $ Message.AuthTestRequest name
                ]
            ]
        , HH.div
            [ HCSS.style do
                CSS.height $ CSS.pct 100.0
                CSS.display CSS.flex
            ]
            [ HH.div
                [ HP.id "side-panel"
                , HCSS.style do
                    CSS.width $ CSS.rem 10.0
                    CSS.height $ CSS.pct 100.0
                    CSS.key (CSS.fromString "resize") "horizontal"
                    traverse_ (CSS.borderRight CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#ddd"
                    CSSOverflow.overflow CSSOverflow.overflowAuto
                ]
                [ HH.slot
                    _games
                    unit
                    gamesComponent
                    (activePlayerId /\ games)
                    \gameId -> when (Maybe.Just gameId /= watchingGameId) do
                      Maybe.maybe (pure unit) (\oldGameId -> Hooks.raise outputToken $ Message.UnsubscribeRequest oldGameId) watchingGameId
                      Hooks.put watchingGameIdId $ Maybe.Just gameId
                      Hooks.raise outputToken $ Message.SubscribeRequest gameId
                , HH.slot
                    _openGames
                    unit
                    openGamesComponent
                    (activePlayerId /\ openGames)
                    \gameId -> Hooks.raise outputToken $ Message.JoinRequest gameId
                ]
            , HH.div
                [ HCSS.style $ CSS.flexGrow 2.0
                ]
                [ case activeGame of
                    Maybe.Just (gameId /\ fields) ->
                      HH.slot
                        _field
                        unit
                        fieldComponent
                        fields
                        \(Click (Tuple x y)) ->
                          let
                            game = Map.lookup gameId games
                          in
                            when (Maybe.isJust activePlayerId && (map _.redPlayerId game == activePlayerId || map _.blackPlayerId game == activePlayerId))
                              $ Hooks.raise outputToken
                              $ Message.PutPointRequest gameId { x, y }
                    Maybe.Nothing ->
                      HH.slot
                        _createGame
                        unit
                        createGameComponent
                        (activePlayerId /\ openGames)
                        case _ of
                          CreateGame width height -> Hooks.raise outputToken $ Message.CreateRequest { width, height }
                          CloseGame gameId -> Hooks.raise outputToken $ Message.CloseRequest gameId
                ]
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
