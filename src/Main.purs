module Main where

import Prelude

import CSS as CSS
import CSS.Common as CSSCommon
import CSS.Cursor as CSSCursor
import CSS.Overflow as CSSOverflow
import CSS.TextAlign as CSSTextAlign
import CSS.VerticalAlign as CSSVerticalAlign
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Monad.Trans.Class (lift)
import Countdown (countdown)
import Data.Argonaut (decodeJson, encodeJson, parseJson, stringify)
import Data.Array as Array
import Data.DateTime.Instant (Instant)
import Data.DateTime.Instant as Instant
import Data.Either as Either
import Data.Foldable (for_, traverse_)
import Data.Int as Int
import Data.List.NonEmpty as NonEmptyList
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Newtype (unwrap, wrap)
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Effect.Now as Now
import Effect.Ref (Ref)
import Effect.Ref as Ref
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
import Halogen.Svg.Attributes as SvgAttributes
import Halogen.Svg.Attributes.StrokeLineCap (StrokeLineCap(..))
import Halogen.Svg.Elements as SvgElements
import Halogen.VDom.Driver (runUI)
import Message as Message
import Player as Player
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
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.URL.URLSearchParams as URLSearchParams

testBuild :: Boolean
testBuild = false

foreign import setCookie :: String -> Effect Unit
foreign import postMessage :: forall m. Window -> m -> Effect Unit
foreign import eventData :: forall d. Event -> d

refresh :: Effect Unit
refresh = HTML.window >>= Window.location >>= Location.reload

-- TODO: refresh on error: https://stackoverflow.com/questions/14787480/page-refresh-in-case-of-javascript-errors
wsProducer :: Ref WS.WebSocket -> Effect WS.WebSocket -> CR.Producer Message.Response Aff Unit
wsProducer socketRef socketEffect = CRA.produce \emitter ->
  let
    addListeners = do
      messageListener <- EET.eventListener \ev ->
        for_ (ME.fromEvent ev) \msgEvent ->
          for_ (Either.either (const Maybe.Nothing) Maybe.Just $ runExcept $ readString $ ME.data_ msgEvent) \msg -> do
            response <- Either.either
              (\e -> liftEffect $ Exception.throwException $ Exception.error $ "Invalid response: " <> show e)
              pure
              (parseJson msg >>= decodeJson)
            emit emitter response
      closeListener <- EET.eventListener $ const do
        Console.warn "WebSocket is disconnected"
        Aff.launchAff_ do
          Aff.delay (Milliseconds 1000.0)
          liftEffect do
            newSocket <- socketEffect
            Ref.write newSocket socketRef
            addListeners
      socket <- Ref.read socketRef
      let eventTarget = WS.toEventTarget socket
      EET.addEventListener
        WSET.onMessage
        messageListener
        false
        eventTarget
      EET.addEventListener
        WSET.onClose
        closeListener
        true
        eventTarget
  in
    addListeners

wsSender :: Ref WS.WebSocket -> Message.Request -> Effect Unit
wsSender socket message = Ref.read socket >>= \s -> WS.sendString s $ stringify $ encodeJson message

_games :: Proxy "games"
_games = Proxy

gamesComponent
  :: forall query m
   . MonadAff m
  => H.Component query (Maybe Message.PlayerId /\ Message.Games) Message.GameId m
gamesComponent =
  Hooks.component \{ outputToken } (activePlayerId /\ games) -> Hooks.do
    Hooks.pure $ HH.div_
      $
        [ HH.div
            [ HCSS.style do
                traverse_ (CSS.borderBottom CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#ddd"
                traverse_ CSS.backgroundColor $ CSS.fromHexString "#f0f0f0"
                traverse_ CSS.color $ CSS.fromHexString "#333"
                CSS.padding (CSS.px 2.0) (CSS.px 2.0) (CSS.px 2.0) (CSS.px 2.0)
                CSSTextAlign.textAlign CSSTextAlign.center
            ]
            [ HH.text "Games"
            ]
        ] <>
          ( map
              ( \(Tuple gameId { redPlayer, blackPlayer, config }) -> HH.div
                  [ HCSS.style do
                      traverse_ (CSS.borderBottom CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#ddd"
                      traverse_ CSS.color $ CSS.fromHexString "#333"
                      CSS.padding (CSS.px 2.0) (CSS.px 2.0) (CSS.px 2.0) (CSS.px 2.0)
                      CSS.cursor CSSCursor.pointer
                  , HE.onClick $ const $ Hooks.raise outputToken gameId
                  ]
                  [ HH.text $ redPlayer.nickname <> " : " <> blackPlayer.nickname <> ", " <> show config.size.width <> "x" <> show config.size.height ]
              )
              $ Map.toUnfoldableUnordered games
          )

_openGames :: Proxy "openGames"
_openGames = Proxy

openGamesComponent
  :: forall query m
   . MonadAff m
  => H.Component query (Maybe Message.PlayerId /\ Message.OpenGames) Message.GameId m
openGamesComponent =
  Hooks.component \{ outputToken } (activePlayerId /\ openGames) -> Hooks.do
    Hooks.pure $ HH.div_
      $
        [ HH.div
            [ HCSS.style do
                traverse_ (CSS.borderBottom CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#ddd"
                traverse_ CSS.backgroundColor $ CSS.fromHexString "#f0f0f0"
                traverse_ CSS.color $ CSS.fromHexString "#333"
                CSS.padding (CSS.px 2.0) (CSS.px 2.0) (CSS.px 2.0) (CSS.px 2.0)
                CSSTextAlign.textAlign CSSTextAlign.center
            ]
            [ HH.text "Open games"
            ]
        ] <>
          ( map
              ( \(Tuple gameId { player, config }) -> HH.div
                  [ HCSS.style do
                      traverse_ (CSS.borderBottom CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#ddd"
                      traverse_ CSS.color $ CSS.fromHexString "#333"
                      CSS.padding (CSS.px 2.0) (CSS.px 2.0) (CSS.px 2.0) (CSS.px 2.0)
                      CSS.cursor CSSCursor.pointer
                  , HE.onClick $ const $ when (Maybe.isJust activePlayerId && map _.playerId (Map.lookup gameId openGames) /= activePlayerId) $
                      Hooks.raise outputToken gameId
                  ]
                  [ HH.text $ player.nickname <> ", " <> show config.size.width <> "x" <> show config.size.height ]
              )
              $ Map.toUnfoldableUnordered openGames
          )

_players :: Proxy "players"
_players = Proxy

playersComponent
  :: forall query output m
   . MonadAff m
  => H.Component query Message.Players output m
playersComponent =
  Hooks.component \_ players -> Hooks.do
    Hooks.pure $ HH.div_
      $
        [ HH.div
            [ HCSS.style do
                traverse_ (CSS.borderBottom CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#ddd"
                traverse_ CSS.backgroundColor $ CSS.fromHexString "#f0f0f0"
                traverse_ CSS.color $ CSS.fromHexString "#333"
                CSS.padding (CSS.px 2.0) (CSS.px 2.0) (CSS.px 2.0) (CSS.px 2.0)
                CSSTextAlign.textAlign CSSTextAlign.center
            ]
            [ HH.text "Players"
            ]
        ] <>
          ( map
              ( \(Tuple _ { nickname }) -> HH.div
                  [ HCSS.style do
                      traverse_ (CSS.borderBottom CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#ddd"
                      traverse_ CSS.color $ CSS.fromHexString "#333"
                      CSS.padding (CSS.px 2.0) (CSS.px 2.0) (CSS.px 2.0) (CSS.px 2.0)
                  ]
                  [ HH.text nickname ]
              )
              $ Map.toUnfoldableUnordered players
          )

buttonStyle :: CSS.CSS
buttonStyle = do
  traverse_ CSS.color $ CSS.fromHexString "#333"
  CSS.key (CSS.fromString "border") "none"
  CSS.padding (CSS.px 5.0) (CSS.px 10.0) (CSS.px 5.0) (CSS.px 10.0)
  CSS.borderRadius (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0)
  CSS.cursor CSSCursor.pointer
  CSS.key (CSS.fromString "white-space") "nowrap"

_createGame :: Proxy "createGame"
_createGame = Proxy

data CreateGameOutput = CreateGame Message.GameConfig | CloseGame Message.GameId

createGameComponent
  :: forall query m
   . MonadAff m
  => H.Component query (Maybe Message.PlayerId /\ Message.OpenGames) CreateGameOutput m
createGameComponent =
  Hooks.component \{ outputToken } (activePlayerId /\ openGames) -> Hooks.do
    Hooks.pure $ HH.div
      [ HCSS.style do
          CSS.width $ CSS.pct 100.0
          CSS.height $ CSS.pct 100.0
          CSS.position CSS.relative
      ]
      [ HH.div
          [ HCSS.style do
              CSS.position CSS.absolute
              CSS.top $ CSS.pct 50.0
              CSS.left $ CSS.pct 50.0
              CSS.transform $ CSS.translate (CSS.pct (-50.0)) (CSS.pct (-50.0))
          ]
          [ if Maybe.isJust activePlayerId then
              case Array.find (\(Tuple _ { playerId }) -> Maybe.Just playerId == activePlayerId) $ Map.toUnfoldable openGames of
                Maybe.Nothing ->
                  HH.button
                    [ HCSS.style buttonStyle
                    , HE.onClick $ const $ Hooks.raise outputToken $ CreateGame { size: { width: 39, height: 32 }, time: { total: 300, increment: 5 } }
                    ]
                    [ HH.text "Create game" ]
                Maybe.Just (Tuple gameId _) ->
                  HH.button
                    [ HCSS.style buttonStyle
                    , HE.onClick $ const $ Hooks.raise outputToken $ CloseGame gameId
                    ]
                    [ HH.text "Cancel game" ]
            else
              HH.label
                [ HCSS.style do
                    CSS.fontSize (CSS.rem 2.0)
                    traverse_ CSS.color $ CSS.fromHexString "#333"
                ]
                [ HH.text "Sign in to play!" ]
          ]
      ]

_signin :: Proxy "signin"
_signin = Proxy

data SignInOutput = SignIn Boolean | SignInTest String

signinComponent
  :: forall query input m
   . MonadAff m
  => H.Component query input SignInOutput m
signinComponent =
  Hooks.component \{ outputToken } _ -> Hooks.do
    let
      rememebrMeEff =
        liftEffect $ do
          window <- HTML.window
          document <- Window.document window
          maybeInput <- map (_ >>= HTMLInputElement.fromElement) $ getElementById "remember-me" (Document.toNonElementParentNode document)
          Maybe.maybe (pure false) HTMLInputElement.checked maybeInput
    Hooks.pure $ HH.div
      [ HP.id "menu"
      , HCSS.style $ CSS.position $ CSS.relative
      ]
      [ HH.fromPlainHTML svgMenu
      , HH.div
          [ HP.id "menu-list"
          , HCSS.style do
              CSS.position CSS.absolute
              CSS.top $ CSS.pct 100.0
              CSS.right $ CSS.px 0.0
              traverse_ CSS.backgroundColor $ CSS.fromHexString "#fff"
              traverse_ (CSS.border CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#ddd"
              CSS.padding (CSS.px 10.0) (CSS.px 10.0) (CSS.px 10.0) (CSS.px 10.0)
              CSS.zIndex 1
          ] $
          [ if not testBuild then
              HH.div
                [ HCSS.style do
                    traverse_ (CSS.borderBottom CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#ddd"
                ]
                [ HH.button
                    [ HP.class_ $ wrap "menu-item"
                    , HCSS.style buttonStyle
                    , HE.onClick $ const $ rememebrMeEff >>= (Hooks.raise outputToken <<< SignIn)
                    ]
                    [ HH.text "Sign in" ]
                ]
            else
              HH.div
                [ HCSS.style do
                    traverse_ (CSS.borderBottom CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#ddd"
                ]
                [ HH.input
                    [ HP.id "test-name"
                    , HCSS.style do
                        CSS.width $ CSS.rem 4.0
                        CSS.margin (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0)
                    , HE.onKeyDown $ \e -> when (KeyboardEvent.key e == "Enter") do
                        maybeName <- liftEffect do
                          window <- HTML.window
                          document <- Window.document window
                          maybeInput <- map (_ >>= HTMLInputElement.fromElement) $ getElementById "test-name" (Document.toNonElementParentNode document)
                          traverse HTMLInputElement.value maybeInput
                        for_ maybeName \name -> do
                          Hooks.raise outputToken $ SignInTest name
                    ]
                ]
          , HH.div
              [ HCSS.style do
                  CSS.display CSS.flex
                  CSS.alignItems CSSCommon.center
                  CSS.padding (CSS.px 2.0) (CSS.px 0.0) (CSS.px 2.0) (CSS.px 0.0)
              ]
              [ HH.input [ HP.id "remember-me", HP.type_ HP.InputCheckbox, HP.checked true ]
              , HH.label
                  [ HP.for "remember-me"
                  , HCSS.style $ do
                      CSS.fontSize (CSS.rem 0.75)
                      CSS.key (CSS.fromString "white-space") "nowrap"
                      traverse_ CSS.color $ CSS.fromHexString "#333"
                  ]
                  [ HH.text "Remember me" ]
              ]
          ]
      ]

_menu :: Proxy "menu"
_menu = Proxy

data MenuOutput = SignOut

menuComponent
  :: forall query m
   . MonadAff m
  => H.Component query Message.Player MenuOutput m
menuComponent =
  Hooks.component \{ outputToken } player -> Hooks.do
    Hooks.pure $ HH.div
      [ HP.id "menu"
      , HCSS.style $ CSS.position $ CSS.relative
      ]
      [ HH.button
          [ HP.id "menu-btn", HCSS.style buttonStyle ]
          [ HH.text player.nickname ]
      , HH.div
          [ HP.id "menu-list"
          , HCSS.style do
              CSS.position CSS.absolute
              CSS.top $ CSS.pct 100.0
              CSS.right $ CSS.px 0.0
              traverse_ CSS.backgroundColor $ CSS.fromHexString "#fff"
              traverse_ (CSS.border CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#ddd"
              CSS.padding (CSS.px 10.0) (CSS.px 10.0) (CSS.px 10.0) (CSS.px 10.0)
              CSS.zIndex 1
          ]
          [ HH.div_
              [ HH.button
                  [ HP.class_ $ wrap "menu-item"
                  , HCSS.style buttonStyle
                  , HE.onClick $ const $ Hooks.raise outputToken SignOut
                  ]
                  [ HH.text "Sign out" ]
              ]
          ]
      ]

_field :: Proxy "field"
_field = Proxy

data AppQuery a = AppQuery Message.Response a

type AppInput =
  { playerId :: Maybe Message.PlayerId
  , players :: Message.Players
  , openGames :: Message.OpenGames
  , games :: Message.Games
  , now :: Instant
  }

svgDot :: String -> HH.PlainHTML
svgDot color = SvgElements.svg
  [ SvgAttributes.width 16.0
  , SvgAttributes.height 16.0
  ]
  [ SvgElements.circle
      [ HP.attr (wrap "shape-rendering") "geometricPrecision"
      , SvgAttributes.cx 8.0
      , SvgAttributes.cy 8.0
      , SvgAttributes.r 4.0
      , SvgAttributes.fill $ SvgAttributes.Named color
      ]
  ]

svgMenu :: HH.PlainHTML
svgMenu = SvgElements.svg
  [ SvgAttributes.id "hamburger"
  , SvgAttributes.width 32.0
  , SvgAttributes.height 32.0
  , HCSS.style $ CSSVerticalAlign.verticalAlign CSSVerticalAlign.Middle
  ]
  let
    line c y =
      [ SvgAttributes.classes [ wrap "line", wrap c ]
      , SvgAttributes.x1 4.0
      , SvgAttributes.y1 y
      , SvgAttributes.x2 28.0
      , SvgAttributes.y2 y
      , SvgAttributes.stroke $ SvgAttributes.Named "dimgrey"
      , SvgAttributes.strokeWidth 4.0
      , SvgAttributes.strokeLineCap LineCapRound
      ]
  in
    [ SvgElements.line $ line "top" 8.0
    , SvgElements.line $ line "middle" 16.0
    , SvgElements.line $ line "bottom" 24.0
    ]

appComponent
  :: forall m
   . MonadAff m
  => H.Component AppQuery AppInput Message.Request m
appComponent =
  Hooks.component \{ queryToken, outputToken, slotToken } input -> Hooks.do
    activePlayerId /\ activePlayerIdId <- Hooks.useState input.playerId
    openGames /\ openGamesId <- Hooks.useState input.openGames
    games /\ gamesId <- Hooks.useState input.games
    players /\ playersId <- Hooks.useState input.players
    watchingGameId /\ watchingGameIdId <- Hooks.useState Maybe.Nothing
    activeGame /\ activeGameId <- Hooks.useState Maybe.Nothing
    now /\ nowId <- Hooks.useState input.now

    Hooks.captures {} Hooks.useTickEffect do
      newNow <- liftEffect $ Now.now
      Hooks.put nowId newNow
      pure Maybe.Nothing

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

    let
      switchToGame gameId = do
        Maybe.maybe (pure unit) (\oldGameId -> Hooks.raise outputToken $ Message.UnsubscribeRequest oldGameId) watchingGameId
        Hooks.put watchingGameIdId $ Maybe.Just gameId
        Hooks.raise outputToken $ Message.SubscribeRequest gameId

    Hooks.useQuery queryToken case _ of
      AppQuery response a -> do
        case response of
          Message.InitResponse playerIdInput playersInput openGamesInput gamesInput -> do
            Hooks.put activePlayerIdId playerIdInput
            Hooks.put openGamesId openGamesInput
            Hooks.put gamesId gamesInput
            Hooks.put playersId playersInput
            Maybe.maybe (pure unit)
              ( \gameId ->
                  if Map.member gameId gamesInput then
                    Hooks.raise outputToken $ Message.SubscribeRequest gameId
                  else
                    -- Should we switch here if games are persisted?
                    Hooks.put watchingGameIdId Maybe.Nothing
              )
              watchingGameId
          Message.GameInitResponse gameId game moves initTime drawOffer timeLeft ->
            if Maybe.Just gameId == watchingGameId then
              Hooks.put activeGameId $ Maybe.Just $ gameId /\ game.config /\ game.redPlayer /\ game.blackPlayer /\ initTime /\ timeLeft /\ Array.foldl
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
          Message.AuthResponse playerId cookie -> do
            Hooks.put activePlayerIdId $ Maybe.Just playerId
            liftEffect $ setCookie cookie
          Message.PlayerJoinedResponse playerId player ->
            Hooks.modify_ playersId $ Map.insert playerId player
          Message.PlayerLeftResponse playerId ->
            Hooks.modify_ playersId $ Map.delete playerId
          Message.CreateResponse gameId openGame ->
            Hooks.modify_ openGamesId $ Map.insert gameId openGame
          Message.CloseResponse gameId ->
            Hooks.modify_ openGamesId $ Map.delete gameId
          Message.StartResponse gameId game -> do
            Hooks.modify_ openGamesId $ Map.delete gameId
            Hooks.modify_ gamesId $ Map.insert gameId game
            when (activePlayerId == Maybe.Just game.redPlayerId || activePlayerId == Maybe.Just game.blackPlayerId) $ switchToGame gameId
          Message.PutPointResponse gameId move puttingTime timeLeft ->
            case activeGame of
              Maybe.Just (activeGameId' /\ config /\ redPlayer /\ blackPlayer /\ _ /\ _ /\ fields) | gameId == activeGameId' ->
                Hooks.put activeGameId
                  $ Maybe.Just
                  $ (/\) activeGameId'
                  $ (/\) config
                  $ (/\) redPlayer
                  $ (/\) blackPlayer
                  $ (/\) puttingTime
                  $ (/\) timeLeft
                  $ Maybe.maybe fields (_ `NonEmptyList.cons` fields)
                  $ Field.putPoint (Tuple move.coordinate.x move.coordinate.y) (unwrap move.player)
                  $ NonEmptyList.head fields
              _ ->
                liftEffect $ Console.warn $ "Wrong game to put point"
          Message.DrawResponse _ _ ->
            pure unit
          Message.GameResultResponse _ _ ->
            pure unit
        pure $ Maybe.Just a

    Hooks.pure
      $ HH.div
          [ HCSS.style do
              CSS.width $ CSS.vw 100.0
              CSS.height $ CSS.vh 100.0
              CSS.display CSS.flex
              CSS.flexDirection CSS.column
          ]
      $
        [ HH.div
            [ HCSS.style do
                CSS.position CSS.relative
                CSS.display CSS.flex
                CSS.justifyContent CSS.spaceBetween
                CSS.alignItems CSSCommon.center
                CSS.padding (CSS.rem 0.5) (CSS.rem 0.5) (CSS.rem 0.5) (CSS.rem 0.5)
                traverse_ CSS.backgroundColor $ CSS.fromHexString "#f2f2f2"
                traverse_ (CSS.borderBottom CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#ddd"
            ]
            [ HH.div
                [ HCSS.style $ CSS.cursor CSSCursor.pointer
                , HE.onClick $ const do
                    Maybe.maybe (pure unit) (\oldGameId -> Hooks.raise outputToken $ Message.UnsubscribeRequest oldGameId) watchingGameId
                    Hooks.put watchingGameIdId Maybe.Nothing
                    Hooks.put activeGameId Maybe.Nothing
                ]
                [ HH.img
                    [ HCSS.style $ CSSVerticalAlign.verticalAlign CSSVerticalAlign.Middle
                    , HP.src "logo.svg"
                    , HP.width 24
                    ]
                , HH.label
                    [ HCSS.style do
                        CSS.cursor CSSCursor.pointer
                        CSSVerticalAlign.verticalAlign CSSVerticalAlign.Middle
                        CSS.padding (CSS.px 5.0) (CSS.px 10.0) (CSS.px 5.0) (CSS.px 10.0)
                        traverse_ CSS.color $ CSS.fromHexString "#333"
                    ]
                    [ HH.text "Kropki"
                    ]
                ]
            , HH.div
                [ HCSS.style do
                    CSS.alignItems CSSCommon.center
                    CSS.display CSS.flex
                    CSS.position CSS.absolute
                    CSS.top (CSS.pct 50.0)
                    CSS.left (CSS.pct 50.0)
                    CSS.transform $ CSS.translate (CSS.pct (-50.0)) (CSS.pct (-50.0))
                    traverse_ CSS.color $ CSS.fromHexString "#333"
                ] $ case activeGame of
                Maybe.Just (_ /\ _ /\ redPlayer /\ blackPlayer /\ puttingTime /\ timeLeft /\ fields) ->
                  let
                    nextPlayer = Field.nextPlayer $ NonEmptyList.head fields
                    redTicking = nextPlayer == Player.Red
                  in
                    [ HH.fromPlainHTML $ countdown redTicking now puttingTime (Milliseconds $ Int.toNumber timeLeft.red)
                    , HH.fromPlainHTML $ svgDot "red"
                    , HH.label_
                        [ HH.text $ redPlayer.nickname <> " : " <> blackPlayer.nickname ]
                    , HH.fromPlainHTML $ svgDot "black"
                    , HH.fromPlainHTML $ countdown (not redTicking) now puttingTime (Milliseconds $ Int.toNumber timeLeft.black)
                    ]
                Maybe.Nothing -> []
            , HH.div
                [ HCSS.style $ CSS.marginLeft CSSCommon.auto
                ]
                case activePlayerId >>= \playerId -> Map.lookup playerId players of
                  Maybe.Nothing ->
                    [ HH.slot
                        _signin
                        unit
                        signinComponent
                        unit
                        case _ of
                          SignIn rememberMe -> Hooks.raise outputToken $ Message.GetAuthUrlRequest rememberMe
                          SignInTest name -> Hooks.raise outputToken $ Message.AuthTestRequest name
                    ]
                  Maybe.Just player ->
                    [ HH.slot
                        _menu
                        unit
                        menuComponent
                        player
                        case _ of
                          SignOut -> do
                            Hooks.raise outputToken Message.SignOutRequest
                            Hooks.put activePlayerIdId Maybe.Nothing
                            liftEffect $ setCookie "kropki=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;"
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
                    \gameId -> when (Maybe.Just gameId /= watchingGameId) $ switchToGame gameId
                , HH.slot
                    _openGames
                    unit
                    openGamesComponent
                    (activePlayerId /\ openGames)
                    \gameId -> Hooks.raise outputToken $ Message.JoinRequest gameId
                , HH.slot_
                    _players
                    unit
                    playersComponent
                    players
                ]
            , HH.div
                [ HCSS.style do
                    CSS.backgroundColor CSS.white
                    CSS.flexGrow 2.0
                    CSS.padding (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                ]
                [ case activeGame of
                    Maybe.Just (gameId /\ config /\ redPlayer /\ blackPlayer /\ puttingTime /\ timeLeft /\ fields) ->
                      let
                        game = Map.lookup gameId games
                        nextPlayer = Field.nextPlayer $ NonEmptyList.head fields
                        pointer = Maybe.isJust activePlayerId &&
                          ( map _.redPlayerId game == activePlayerId && nextPlayer == Player.Red ||
                              map _.blackPlayerId game == activePlayerId && nextPlayer == Player.Black
                          )
                      in
                        HH.slot
                          _field
                          unit
                          fieldComponent
                          { fields, pointer }
                          \(Click (Tuple x y)) -> when pointer do
                            now' <- liftEffect $ Now.now
                            let
                              elapsed :: Milliseconds
                              elapsed = Instant.diff now' puttingTime
                              diff = Int.ceil (unwrap elapsed)
                            Hooks.put activeGameId
                              $ Maybe.Just
                              $ (/\) gameId
                              $ (/\) config
                              $ (/\) redPlayer
                              $ (/\) blackPlayer
                              $ (/\) now'
                              $ (/\)
                                  { red: max 0 (timeLeft.red - if nextPlayer == Player.Red then diff else 0)
                                  , black: max 0 (timeLeft.black - if nextPlayer == Player.Black then diff else 0)
                                  }
                              $ Maybe.maybe fields (_ `NonEmptyList.cons` fields)
                              $ Field.putPoint (Tuple x y) nextPlayer
                              $ NonEmptyList.head fields
                            Hooks.raise outputToken $ Message.PutPointRequest gameId { x, y }
                    Maybe.Nothing ->
                      HH.slot
                        _createGame
                        unit
                        createGameComponent
                        (activePlayerId /\ openGames)
                        case _ of
                          CreateGame config -> Hooks.raise outputToken $ Message.CreateRequest config
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
    let connectionEffect = WS.create "ws://127.0.0.1:8080" []
    connection <- connectionEffect
    connectionRef <- Ref.new connection
    listener <- EET.eventListener \event -> do
      let data' = eventData event
      wsSender connectionRef $ Message.AuthRequest data'.code data'.state
    EET.addEventListener (wrap "message") listener true $ Window.toEventTarget window
    HA.runHalogenAff do
      body <- HA.awaitBody
      CR.runProcess $ wsProducer connectionRef connectionEffect CR.$$ do
        input <- CR.await >>= case _ of
          Message.InitResponse playerId players openGames games -> do
            now <- liftEffect $ Now.now
            pure { playerId, players, openGames, games, now }
          other -> lift $ liftEffect $ Exception.throwException $ Exception.error $ "Unexpected first message: " <> show other
        io <- lift $ runUI appComponent input body
        _ <- H.liftEffect $ HS.subscribe io.messages $ wsSender connectionRef
        CR.consumer \response -> (io.query $ H.mkTell $ AppQuery response) *> pure Maybe.Nothing
