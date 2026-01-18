module Main where

import Prelude

import CSS as CSS
import CSS.Box as CSSBox
import CSS.Common as CSSCommon
import CSS.Cursor as CSSCursor
import CSS.Font as CSSFont
import CSS.FontStyle as CSSFontStyle
import CSS.Overflow as CSSOverflow
import CSS.Text.Transform as CSSTextTransform
import CSS.TextAlign as CSSTextAlign
import CSS.VerticalAlign as CSSVerticalAlign
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (Except, runExcept)
import Control.Monad.Maybe.Trans (MaybeT(..), mapMaybeT, runMaybeT)
import Control.Monad.Trans.Class (lift)
import Countdown (countdown)
import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Argonaut (decodeJson, encodeJson, parseJson, stringify)
import Data.Array as Array
import Data.DateTime.Instant as Instant
import Data.Either as Either
import Data.Foldable (elem, for_, traverse_)
import Data.Int as Int
import Data.List.NonEmpty as NonEmptyList
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Newtype (unwrap, wrap)
import Data.NonEmpty as NonEmpty
import Data.Number as Number
import Data.Time.Duration (Milliseconds(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Effect.Now as Now
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Field as Field
import FieldComponent (fieldComponent, Output(..), Redraw(..))
import Foreign (Foreign, ForeignError, readString, unsafeToForeign, unsafeFromForeign, isNull, isUndefined)
import Foreign.Index ((!))
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
import Render (DrawSettings, defaultDrawSettings)
import Type.Proxy (Proxy(..))
import UsePersistentState as UsePersistentState
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Element as Element
import Web.DOM.MutationObserver as MutationObserver
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event (Event)
import Web.Event.EventTarget as EET
import Web.HTML as HTML
import Web.HTML.Event.PopStateEvent as PopStateEvent
import Web.HTML.HTMLDocument as Document
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.History as History
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
foreign import eventData :: Event -> Foreign

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

formatConfig :: Message.GameConfig -> HH.PlainHTML
formatConfig config =
  let
    widthStr = show config.size.width
    heightStr = show config.size.height
    minutes = config.time.total / 60
    seconds = config.time.total `mod` 60
    secPad = if seconds < 10 then "0" else ""
    timeStr = show minutes <> ":" <> secPad <> show seconds <> "+" <> show config.time.increment
  in
    HH.span_
      [ HH.text $ widthStr <> "x" <> heightStr
      , HH.br_
      , HH.text timeStr
      ]

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
            [ HP.class_ $ wrap rosterHeaderClass ]
            [ HH.text "Games" ]
        ] <>
          ( map
              ( \(Tuple gameId { redPlayer, blackPlayer, config }) -> HH.div
                  [ HP.classes [ wrap rosterItemRowClass, wrap clickableClass ]
                  , HE.onClick $ const $ Hooks.raise outputToken gameId
                  ]
                  [ HH.div
                      [ HP.class_ $ wrap rosterNameClass ]
                      [ HH.text redPlayer.nickname
                      , HH.span
                          [ HCSS.style do
                              traverse_ CSS.color $ CSS.fromHexString "#bbb"
                              CSS.padding (CSS.px 0.0) (CSS.px 2.0) (CSS.px 0.0) (CSS.px 2.0)
                              CSS.fontStyle CSSFontStyle.Italic
                          ]
                          [ HH.text "vs" ]
                      , HH.text blackPlayer.nickname
                      ]
                  , HH.div
                      [ HP.class_ $ wrap rosterMetaClass ]
                      [ HH.fromPlainHTML $ formatConfig config ]
                  ]
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
            [ HP.class_ $ wrap rosterHeaderClass ]
            [ HH.text "Open games" ]
        ] <>
          ( map
              ( \(Tuple gameId { playerId, player, config }) -> HH.div
                  [ HP.classes
                      if Maybe.isNothing activePlayerId || elem playerId activePlayerId then
                        [ wrap rosterItemRowClass ]
                      else
                        [ wrap rosterItemRowClass, wrap clickableClass ]
                  , HE.onClick $ const $ when (Maybe.isJust activePlayerId && map _.playerId (Map.lookup gameId openGames) /= activePlayerId) $
                      Hooks.raise outputToken gameId
                  ]
                  [ HH.div
                      [ HP.class_ $ wrap rosterNameClass ]
                      [ HH.text player.nickname ]
                  , HH.div
                      [ HP.class_ $ wrap rosterMetaClass ]
                      [ HH.fromPlainHTML $ formatConfig config ]
                  ]
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
            [ HP.class_ $ wrap rosterHeaderClass
            ]
            [ HH.text "Players" ]
        ] <>
          ( map
              ( \(Tuple _ player) -> HH.div
                  [ HP.class_ $ wrap rosterItemRowClass ]
                  [ HH.div
                      [ HP.class_ $ wrap rosterNameClass ]
                      [ HH.text player.nickname ]
                  , HH.div
                      [ HP.class_ $ wrap rosterMetaClass ]
                      [ HH.text "1500" ]
                  ]
              )
              $ Map.toUnfoldableUnordered players
          )

buttonStyle :: CSS.CSS
buttonStyle = do
  CSS.width (CSS.pct 100.0)
  traverse_ CSS.color $ CSS.fromHexString "#333"
  CSS.key (CSS.fromString "border") "none"
  CSS.padding (CSS.px 5.0) (CSS.px 10.0) (CSS.px 5.0) (CSS.px 10.0)
  CSS.borderRadius (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0)
  CSS.cursor CSSCursor.pointer
  CSS.key (CSS.fromString "white-space") "nowrap"

_profileSettings :: Proxy "profileSettings"
_profileSettings = Proxy

data Availability
  = AvailabilityInit
  | AvailabilityChecking String
  | AvailabilityAvailable String
  | AvailabilityTaken String

derive instance eqAvailability :: Eq Availability

data ProfileOutput
  = CheckNickname String
  | ChangeNickname String

profileSettingsComponent
  :: forall query m
   . MonadAff m
  => H.Component query { currentNickname :: String, availability :: Availability } ProfileOutput m
profileSettingsComponent =
  Hooks.component \{ outputToken } input -> Hooks.do
    -- State for the text input
    nickname /\ nicknameId <- Hooks.useState input.currentNickname
    -- Debounce timer fiber
    fiber /\ fiberId <- Hooks.useState (Maybe.Nothing :: Maybe H.ForkId)

    let
      borderColor = case input.availability of
        AvailabilityInit | nickname == "" || nickname == input.currentNickname -> CSS.fromHexString "#dee2e6"
        AvailabilityAvailable nickname' | nickname == nickname' -> CSS.fromHexString "#28a745" -- Green
        AvailabilityTaken nickname' | nickname == nickname' -> CSS.fromHexString "#dc3545" -- Red
        _ -> CSS.fromHexString "#ffc107" -- Yellow

      renderHeader title =
        HH.h3
          [ HCSS.style do
              CSS.marginTop (CSS.px 0.0)
              CSS.fontSize (CSS.rem 0.9)
              traverse_ CSS.color $ CSS.fromHexString "#666"
              CSS.textTransform CSSTextTransform.uppercase
              CSS.letterSpacing (CSS.px 1.0)
              traverse_ (CSS.borderBottom CSS.solid (CSS.px 1.0)) (CSS.fromHexString "#dee2e6")
              CSS.paddingBottom (CSS.px 10.0)
              CSS.marginBottom (CSS.px 15.0)
          ]
          [ HH.text title ]

      isDirty = nickname /= input.currentNickname
      isValid = isDirty && input.availability == AvailabilityAvailable nickname

    Hooks.pure $ HH.div
      [ HCSS.style do
          CSS.width $ CSS.pct 100.0
          CSS.height $ CSS.pct 100.0
          CSS.position CSS.relative
          CSS.display CSS.flex
          CSS.justifyContent CSSCommon.center
          CSS.alignItems CSSCommon.center
          CSS.backgroundColor CSS.white
      ]
      [ HH.div
          [ HCSS.style do
              traverse_ (CSS.border CSS.solid (CSS.px 1.0)) (CSS.fromHexString "#dee2e6")
              CSS.borderRadius (CSS.px 8.0) (CSS.px 8.0) (CSS.px 8.0) (CSS.px 8.0)
              CSS.padding (CSS.px 30.0) (CSS.px 30.0) (CSS.px 30.0) (CSS.px 30.0)
              CSS.backgroundColor CSS.white
              CSS.boxShadow $ (CSS.rgba 0 0 0 0.05) `CSSBox.bsColor` CSSBox.shadowWithBlur (CSS.px 0.0) (CSS.px 4.0) (CSS.px 12.0) NonEmpty.:| []
          ]
          [ HH.div
              [ HCSS.style $ CSS.marginBottom (CSS.px 30.0) ]
              [ renderHeader "Profile Settings"
              , HH.table_
                  [ HH.tr_
                      [ HH.td
                          [ HCSS.style do
                              CSS.fontSize (CSS.px 14.0)
                              CSS.padding (CSS.px 6.0) (CSS.px 15.0) (CSS.px 6.0) (CSS.px 0.0)
                          ]
                          [ HH.text "Nickname" ]
                      , HH.td
                          [ HCSS.style $ CSS.padding (CSS.px 6.0) (CSS.px 0.0) (CSS.px 6.0) (CSS.px 0.0) ]
                          [ HH.input
                              [ HP.type_ HP.InputText
                              , HP.value nickname
                              , HE.onValueInput \val -> do
                                  Hooks.put nicknameId val

                                  -- Cancel previous timer
                                  traverse_ Hooks.kill fiber

                                  if val == input.currentNickname || val == "" then do
                                    -- Reset if empty or same as original
                                    Hooks.raise outputToken $ CheckNickname "" -- Hack to reset parent state to Init
                                    Hooks.put fiberId Maybe.Nothing
                                  else do
                                    -- Start new timer
                                    newFiber <- Hooks.fork do
                                      liftAff $ Aff.delay $ Milliseconds 500.0
                                      Hooks.raise outputToken $ CheckNickname val
                                    Hooks.put fiberId $ Maybe.Just newFiber
                              , HCSS.style do
                                  CSS.width (CSS.px 200.0)
                                  CSS.padding (CSS.px 6.0) (CSS.px 6.0) (CSS.px 6.0) (CSS.px 6.0)
                                  traverse_ (CSS.border CSS.solid (CSS.px 2.0)) borderColor
                                  CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                                  CSS.outlineStyle $ CSS.fromString "none"
                              ]
                          ]
                      ]
                  ]
              ]
          , HH.div
              [ HCSS.style do
                  CSS.display CSS.flex
                  CSS.justifyContent CSSCommon.center
                  traverse_ (CSS.borderTop CSS.solid (CSS.px 1.0)) (CSS.fromHexString "#dee2e6")
                  CSS.paddingTop (CSS.px 20.0)
              ]
              [ HH.button
                  [ HP.disabled (not isValid)
                  , HCSS.style do
                      traverse_ CSS.backgroundColor $ CSS.fromHexString "#e9ecef"
                      traverse_ (CSS.border CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#dee2e6"
                      CSS.padding (CSS.px 10.0) (CSS.px 25.0) (CSS.px 10.0) (CSS.px 25.0)
                      CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                      CSS.cursor $ if isValid then CSSCursor.pointer else CSSCursor.notAllowed
                      CSS.fontWeight CSS.bold
                      CSS.marginRight (CSS.px 15.0)
                  , HE.onClick $ const $ Hooks.raise outputToken $ ChangeNickname nickname
                  ]
                  [ HH.text "Update Profile" ]
              ]
          ]
      ]

-- Hardcoded moves for the preview field to show off surroundings and points
previewFields :: NonEmptyList.NonEmptyList Field.Field
previewFields =
  let
    empty = Field.emptyField 10 10
    -- A small setup with a Red enclosure capturing Black points
    moves =
      [ -- Captured Black points
        (3 /\ 3) /\ Player.Black
      , (4 /\ 3) /\ Player.Black
      , (3 /\ 4) /\ Player.Black
      -- Capturing red points
      , (2 /\ 2) /\ Player.Red
      , (3 /\ 2) /\ Player.Red
      , (4 /\ 2) /\ Player.Red
      , (5 /\ 3) /\ Player.Red
      , (5 /\ 4) /\ Player.Red
      , (4 /\ 5) /\ Player.Red
      , (3 /\ 5) /\ Player.Red
      , (2 /\ 4) /\ Player.Red
      , (2 /\ 3) /\ Player.Red
      -- Loose points
      , (7 /\ 7) /\ Player.Red
      , (8 /\ 8) /\ Player.Black
      , (1 /\ 8) /\ Player.Black
      , (2 /\ 8) /\ Player.Black
      ]
  in
    Array.foldl
      ( \fields (coord /\ player) ->
          Maybe.maybe fields (_ `NonEmptyList.cons` fields)
            $ Field.putPoint coord player
            $ NonEmptyList.head fields
      )
      (NonEmptyList.singleton empty)
      moves

_previewField :: Proxy "previewField"
_previewField = Proxy

_drawSettings :: Proxy "drawSettings"
_drawSettings = Proxy

data SettingsOutput = UpdateSettings DrawSettings

drawSettingsComponent
  :: forall query m
   . MonadAff m
  => H.Component query DrawSettings SettingsOutput m
drawSettingsComponent =
  Hooks.component \{ outputToken } initialSettings -> Hooks.do
    settings /\ settingsId <- Hooks.useState initialSettings

    let
      borderColor = CSS.fromHexString "#dee2e6"
      primaryBtnColor = CSS.fromHexString "#e9ecef"

      -- Helper to render a generic row (Number/Int/String/Color)
      renderInputRow label inputType val minVal maxVal stepVal onChange =
        HH.tr_
          [ HH.td
              [ HCSS.style do
                  CSS.fontSize (CSS.px 14.0)
                  CSS.padding (CSS.px 6.0) (CSS.px 15.0) (CSS.px 6.0) (CSS.px 0.0)
              ]
              [ HH.text label ]
          , HH.td
              [ HCSS.style $ CSS.padding (CSS.px 6.0) (CSS.px 0.0) (CSS.px 6.0) (CSS.px 0.0) ]
              [ HH.input $
                  [ HP.type_ inputType
                  , HP.value val
                  , HE.onValueInput onChange
                  , HCSS.style do
                      CSS.width (CSS.px 60.0)
                      CSS.padding (CSS.px 2.0) (CSS.px 2.0) (CSS.px 2.0) (CSS.px 2.0)
                      traverse_ (CSS.border CSS.solid (CSS.px 1.0)) borderColor
                      CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                      CSSTextAlign.textAlign CSSTextAlign.center
                  ]
                    <> Maybe.maybe [] (pure <<< HP.min) minVal
                    <> Maybe.maybe [] (pure <<< HP.max) maxVal
                    <>
                      Maybe.maybe [] (pure <<< HP.step) stepVal
              ]
          ]

      -- Helper to render a Checkbox row
      renderCheckboxRow label isChecked onToggle =
        HH.tr_
          [ HH.td
              [ HCSS.style do
                  CSS.fontSize (CSS.px 14.0)
                  CSS.padding (CSS.px 6.0) (CSS.px 15.0) (CSS.px 6.0) (CSS.px 0.0)
              ]
              [ HH.text label ]
          , HH.td
              [ HCSS.style $ CSS.padding (CSS.px 6.0) (CSS.px 0.0) (CSS.px 6.0) (CSS.px 0.0) ]
              [ HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.checked isChecked
                  , HE.onChecked \_ -> onToggle (not isChecked)
                  , HCSS.style do
                      CSS.width (CSS.px 18.0)
                      CSS.height (CSS.px 18.0)
                      CSS.cursor CSSCursor.pointer
                  ]
              ]
          ]

      -- Render Section Header
      renderHeader title =
        HH.h3
          [ HCSS.style do
              CSS.marginTop (CSS.px 0.0)
              CSS.fontSize (CSS.rem 0.9)
              traverse_ CSS.color $ CSS.fromHexString "#666"
              CSS.textTransform CSSTextTransform.uppercase
              CSS.letterSpacing (CSS.px 1.0)
              traverse_ (CSS.borderBottom CSS.solid (CSS.px 1.0)) borderColor
              CSS.paddingBottom (CSS.px 10.0)
              CSS.marginBottom (CSS.px 15.0)
          ]
          [ HH.text title ]

    Hooks.pure $ HH.div
      [ HCSS.style do
          CSS.width $ CSS.pct 100.0
          CSS.height $ CSS.pct 100.0
          CSS.position CSS.relative
          CSS.display CSS.flex
          CSS.justifyContent CSSCommon.center
          CSS.alignItems CSSCommon.center
          CSS.backgroundColor CSS.white
      ]
      [ HH.div
          [ HCSS.style do
              traverse_ (CSS.border CSS.solid (CSS.px 1.0)) borderColor
              CSS.borderRadius (CSS.px 8.0) (CSS.px 8.0) (CSS.px 8.0) (CSS.px 8.0)
              CSS.padding (CSS.px 30.0) (CSS.px 30.0) (CSS.px 30.0) (CSS.px 30.0)
              CSS.backgroundColor CSS.white
              CSS.boxShadow $ (CSS.rgba 0 0 0 0.05) `CSSBox.bsColor` CSSBox.shadowWithBlur (CSS.px 0.0) (CSS.px 4.0) (CSS.px 12.0) NonEmpty.:| []
          ]
          [ -- Main Layout: Settings inputs (left) + Preview (right)
            HH.div
              [ HCSS.style do
                  CSS.display CSS.flex
                  CSS.flexDirection CSS.row
                  CSS.alignItems CSS.flexStart
              ]
              [ -- LEFT COLUMN: Settings Inputs
                HH.div
                  [ HCSS.style do
                      CSS.display CSS.flex
                      CSS.marginBottom (CSS.px 30.0)
                  ]
                  [ -- Sub-column 1: Appearance & Options
                    HH.div
                      [ HCSS.style $ CSS.marginRight (CSS.px 40.0) ]
                      [ renderHeader "Appearance"
                      , HH.table_
                          [ renderInputRow "Grid Thickness" HP.InputNumber (show settings.gridThickness) (Maybe.Just 1.0) (Maybe.Just 5.0) (Maybe.Just $ Step 1.0)
                              \s -> traverse_ (\v -> Hooks.modify_ settingsId _ { gridThickness = v }) (Int.fromString s)
                          , renderInputRow "Point Radius" HP.InputNumber (show settings.pointRadius) (Maybe.Just 0.1) (Maybe.Just 5.0) (Maybe.Just $ Step 0.1)
                              \s -> traverse_ (\v -> Hooks.modify_ settingsId _ { pointRadius = v }) (Number.fromString s)
                          , renderInputRow "Opacity" HP.InputNumber (show settings.fillingAlpha) (Maybe.Just 0.0) (Maybe.Just 1.0) (Maybe.Just $ Step 0.1)
                              \s -> traverse_ (\v -> Hooks.modify_ settingsId _ { fillingAlpha = v }) (Number.fromString s)
                          ]
                      , HH.div [ HCSS.style $ CSS.marginTop (CSS.px 20.0) ] []
                      , renderHeader "Options"
                      , HH.table_
                          [ renderCheckboxRow "Full Fill" settings.fullFill (\v -> Hooks.modify_ settingsId _ { fullFill = v })
                          , renderCheckboxRow "Extended Fill" settings.extendedFill (\v -> Hooks.modify_ settingsId _ { extendedFill = v })
                          , renderCheckboxRow "Inner Surroundings" settings.innerSurroundings (\v -> Hooks.modify_ settingsId _ { innerSurroundings = v })
                          , renderCheckboxRow "H-Reflection" settings.hReflection (\v -> Hooks.modify_ settingsId _ { hReflection = v })
                          , renderCheckboxRow "V-Reflection" settings.vReflection (\v -> Hooks.modify_ settingsId _ { vReflection = v })
                          ]
                      ]
                  , -- Sub-column 2: Colors
                    HH.div_
                      [ renderHeader "Colors"
                      , HH.table_
                          [ renderInputRow "Grid" HP.InputColor settings.gridColor Maybe.Nothing Maybe.Nothing Maybe.Nothing
                              \v -> Hooks.modify_ settingsId _ { gridColor = v }
                          , renderInputRow "Red Player" HP.InputColor settings.redColor Maybe.Nothing Maybe.Nothing Maybe.Nothing
                              \v -> Hooks.modify_ settingsId _ { redColor = v }
                          , renderInputRow "Black Player" HP.InputColor settings.blackColor Maybe.Nothing Maybe.Nothing Maybe.Nothing
                              \v -> Hooks.modify_ settingsId _ { blackColor = v }
                          ]
                      ]
                  ]
              , -- RIGHT COLUMN: Live Preview
                HH.div
                  [ HCSS.style do
                      CSS.marginLeft (CSS.px 40.0)
                      CSS.paddingLeft (CSS.px 40.0)
                      traverse_ (CSS.borderLeft CSS.solid (CSS.px 1.0)) borderColor
                  ]
                  [ renderHeader "Preview"
                  , HH.div
                      [ HCSS.style do
                          CSS.width (CSS.px 300.0)
                          CSS.height (CSS.px 300.0)
                          traverse_ (CSS.border CSS.solid (CSS.px 1.0)) borderColor
                          CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                          CSS.boxShadow $ (CSS.rgba 0 0 0 0.05) `CSSBox.bsColor` CSSBox.shadowWithBlur (CSS.px 0.0) (CSS.px 2.0) (CSS.px 6.0) NonEmpty.:| []
                          CSS.backgroundColor CSS.white
                      ]
                      [ HH.slot
                          _previewField
                          unit
                          fieldComponent
                          { fields: previewFields
                          , pointer: false
                          , drawSettings: settings
                          }
                          (const (pure unit))
                      ]
                  ]
              ]
          , -- Button Group
            HH.div
              [ HCSS.style do
                  CSS.display CSS.flex
                  CSS.justifyContent CSSCommon.center
                  traverse_ (CSS.borderTop CSS.solid (CSS.px 1.0)) borderColor
                  CSS.paddingTop (CSS.px 20.0)
              ]
              [ HH.button
                  [ HCSS.style do
                      traverse_ CSS.backgroundColor primaryBtnColor
                      traverse_ (CSS.border CSS.solid (CSS.px 1.0)) borderColor
                      CSS.padding (CSS.px 10.0) (CSS.px 25.0) (CSS.px 10.0) (CSS.px 25.0)
                      CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                      CSS.cursor CSSCursor.pointer
                      CSS.fontWeight CSS.bold
                  , HE.onClick $ const $ Hooks.raise outputToken $ UpdateSettings settings
                  ]
                  [ HH.text "Update Settings" ]
              ]
          ]
      ]

_createGame :: Proxy "createGame"
_createGame = Proxy

data CreateGameOutput
  = CreateGame Message.GameConfig
  | CreateLocalGame Message.GameConfig
  | CloseGame Message.GameId

createGameComponent
  :: forall query m
   . MonadAff m
  => H.Component query (Maybe Message.PlayerId /\ Message.OpenGames) CreateGameOutput m
createGameComponent =
  Hooks.component \{ outputToken } (activePlayerId /\ openGames) -> Hooks.do
    let
      -- Check if current user has an open game
      existingGame = case activePlayerId of
        Maybe.Just pid -> Array.find (\(Tuple _ { playerId }) -> playerId == pid) (Map.toUnfoldable openGames)
        Maybe.Nothing -> Maybe.Nothing

      -- Calculate display values based on whether we are viewing an existing game or creating a new one
      displayState = case existingGame of
        Maybe.Just (Tuple _ openGame) ->
          let
            totalSeconds = openGame.config.time.total
          in
            { width: show openGame.config.size.width
            , height: show openGame.config.size.height
            , minutes: show (totalSeconds / 60)
            , seconds: show (totalSeconds `mod` 60)
            , increment: show openGame.config.time.increment
            , disabled: true
            }
        Maybe.Nothing ->
          { width: "39"
          , height: "32"
          , minutes: "5"
          , seconds: "0"
          , increment: "5"
          , disabled: false
          }

      borderColor = CSS.fromHexString "#dee2e6"
      primaryBtnColor = CSS.fromHexString "#e9ecef"

      -- Render Helper for Table Rows
      renderRow id label value minVal maxVal isDisabled =
        HH.tr_
          [ HH.td
              [ HCSS.style do
                  CSS.fontSize (CSS.px 14.0)
                  CSS.padding (CSS.px 6.0) (CSS.px 15.0) (CSS.px 6.0) (CSS.px 0.0)
              ]
              [ HH.text label ]
          , HH.td
              [ HCSS.style $ CSS.padding (CSS.px 6.0) (CSS.px 0.0) (CSS.px 6.0) (CSS.px 0.0) ]
              [ HH.input
                  [ HP.id id
                  , HP.type_ HP.InputNumber
                  , HP.value value
                  , HP.min minVal
                  , HP.max maxVal
                  , HP.disabled isDisabled
                  , HCSS.style do
                      CSS.width (CSS.px 60.0)
                      CSS.padding (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0) (CSS.px 5.0)
                      traverse_ (CSS.border CSS.solid (CSS.px 1.0)) borderColor
                      CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                      CSSTextAlign.textAlign CSSTextAlign.center
                  ]
              ]
          ]

      -- Render Section Header
      renderHeader title =
        HH.h3
          [ HCSS.style do
              CSS.marginTop (CSS.px 0.0)
              CSS.fontSize (CSS.rem 0.9)
              traverse_ CSS.color $ CSS.fromHexString "#666"
              CSS.textTransform CSSTextTransform.uppercase
              CSS.letterSpacing (CSS.px 1.0)
              traverse_ (CSS.borderBottom CSS.solid (CSS.px 1.0)) borderColor
              CSS.paddingBottom (CSS.px 10.0)
              CSS.marginBottom (CSS.px 15.0)
          ]
          [ HH.text title ]

    Hooks.pure $ HH.div
      [ HCSS.style do
          CSS.width $ CSS.pct 100.0
          CSS.height $ CSS.pct 100.0
          CSS.position CSS.relative
          CSS.display CSS.flex
          CSS.justifyContent CSSCommon.center
          CSS.alignItems CSSCommon.center
          CSS.backgroundColor CSS.white
      ]
      [ HH.div
          [ HCSS.style do
              traverse_ (CSS.border CSS.solid (CSS.px 1.0)) borderColor
              CSS.borderRadius (CSS.px 8.0) (CSS.px 8.0) (CSS.px 8.0) (CSS.px 8.0)
              CSS.padding (CSS.px 30.0) (CSS.px 30.0) (CSS.px 30.0) (CSS.px 30.0)
              CSS.backgroundColor CSS.white
              CSS.boxShadow $ (CSS.rgba 0 0 0 0.05) `CSSBox.bsColor` CSSBox.shadowWithBlur (CSS.px 0.0) (CSS.px 4.0) (CSS.px 12.0) NonEmpty.:| []
          ]
          [ -- Config Grid
            HH.div
              [ HCSS.style do
                  CSS.display CSS.flex
                  CSS.marginBottom (CSS.px 30.0)
              ]
              [ -- Column 1: Board Size
                HH.div
                  [ HCSS.style $ CSS.marginRight (CSS.px 40.0) ]
                  [ renderHeader "Board Size"
                  , HH.table_
                      [ renderRow "width" "Width" displayState.width 10.0 50.0 displayState.disabled
                      , renderRow "height" "Height" displayState.height 10.0 50.0 displayState.disabled
                      ]
                  ]
              , -- Column 2: Time Control
                HH.div_
                  [ renderHeader "Time Control"
                  , HH.table_
                      [ renderRow "minutes" "Minutes" displayState.minutes 0.0 239.0 displayState.disabled
                      , renderRow "seconds" "Seconds" displayState.seconds 0.0 59.0 displayState.disabled
                      , renderRow "increment" "Increment" displayState.increment 0.0 60.0 displayState.disabled
                      ]
                  ]
              ]
          , -- Button Group
            HH.div
              [ HCSS.style do
                  CSS.display CSS.flex
                  CSS.justifyContent CSSCommon.center
                  traverse_ (CSS.borderTop CSS.solid (CSS.px 1.0)) borderColor
                  CSS.paddingTop (CSS.px 20.0)
              ]
              case existingGame of
                Maybe.Nothing ->
                  [ HH.button
                      [ HP.disabled $ Maybe.isNothing activePlayerId
                      , HP.title $ if Maybe.isNothing activePlayerId then "You must be signed in to create a game" else ""
                      , HCSS.style do
                          traverse_ CSS.backgroundColor primaryBtnColor
                          traverse_ (CSS.border CSS.solid (CSS.px 1.0)) borderColor
                          CSS.padding (CSS.px 10.0) (CSS.px 25.0) (CSS.px 10.0) (CSS.px 25.0)
                          CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                          CSS.cursor $ if Maybe.isNothing activePlayerId then CSSCursor.notAllowed else CSSCursor.pointer
                          CSS.fontWeight CSS.bold
                          CSS.marginRight (CSS.px 15.0)
                      , HE.onClick $ const $ do
                          window <- liftEffect $ HTML.window
                          document <- liftEffect $ Window.document window
                          let
                            document' = Document.toNonElementParentNode document
                            getInput id = mapMaybeT liftEffect $ do
                              input <- wrap $ map (_ >>= HTMLInputElement.fromElement) $ getElementById id document'
                              value <- lift $ HTMLInputElement.value input
                              MaybeT $ pure $ Int.fromString value
                          void $ runMaybeT $ do
                            width <- getInput "width"
                            height <- getInput "height"
                            minutes <- getInput "minutes"
                            seconds <- getInput "seconds"
                            increment <- getInput "increment"
                            let totalTime = (minutes * 60) + seconds
                            lift $ when (totalTime >= 30) $ Hooks.raise outputToken $ CreateGame
                              { size: { width, height }
                              , time: { total: totalTime, increment }
                              }
                      ]
                      [ HH.text "Create game" ]
                  , HH.button
                      [ HCSS.style do
                          traverse_ CSS.backgroundColor primaryBtnColor
                          traverse_ (CSS.border CSS.solid (CSS.px 1.0)) borderColor
                          CSS.padding (CSS.px 10.0) (CSS.px 25.0) (CSS.px 10.0) (CSS.px 25.0)
                          CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                          CSS.cursor CSSCursor.pointer
                          CSS.fontWeight CSS.bold
                      , HE.onClick $ const $ do
                          window <- liftEffect $ HTML.window
                          document <- liftEffect $ Window.document window
                          let
                            document' = Document.toNonElementParentNode document
                            getInput id = mapMaybeT liftEffect $ do
                              input <- wrap $ map (_ >>= HTMLInputElement.fromElement) $ getElementById id document'
                              value <- lift $ HTMLInputElement.value input
                              MaybeT $ pure $ Int.fromString value
                          void $ runMaybeT $ do
                            width <- getInput "width"
                            height <- getInput "height"
                            minutes <- getInput "minutes"
                            seconds <- getInput "seconds"
                            increment <- getInput "increment"
                            let totalTime = (minutes * 60) + seconds
                            lift $ Hooks.raise outputToken $ CreateLocalGame
                              { size: { width, height }
                              , time: { total: totalTime, increment }
                              }
                      ]
                      [ HH.text "Local game" ]
                  ]
                Maybe.Just (Tuple gameId _) ->
                  [ HH.button
                      [ HCSS.style do
                          traverse_ CSS.backgroundColor primaryBtnColor
                          traverse_ (CSS.border CSS.solid (CSS.px 1.0)) borderColor
                          CSS.padding (CSS.px 10.0) (CSS.px 25.0) (CSS.px 10.0) (CSS.px 25.0)
                          CSS.borderRadius (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0) (CSS.px 4.0)
                          CSS.cursor CSSCursor.pointer
                          CSS.fontWeight CSS.bold
                      , HE.onClick $ const $ Hooks.raise outputToken $ CloseGame gameId
                      ]
                      [ HH.text "Cancel game" ]
                  ]
          ]
      ]

_signin :: Proxy "signin"
_signin = Proxy

menuOnClick :: forall e. MonadEffect e => e Unit
menuOnClick = liftEffect $ do
  window <- HTML.window
  document <- Window.document window
  void $ runMaybeT $ do
    menu <- wrap $ getElementById "menu" (Document.toNonElementParentNode document)
    classes <- lift $ Element.classList menu
    contains <- lift $ DOMTokenList.contains classes "closed"
    if contains then
      lift $ DOMTokenList.remove classes "closed"
    else
      lift $ DOMTokenList.add classes "closed"

menuOnMouseLeave :: forall e. MonadEffect e => e Unit
menuOnMouseLeave = liftEffect $ do
  window <- HTML.window
  document <- Window.document window
  void $ runMaybeT $ do
    menu <- wrap $ getElementById "menu" (Document.toNonElementParentNode document)
    classes <- lift $ Element.classList menu
    lift $ DOMTokenList.remove classes "closed"

_menu :: Proxy "menu"
_menu = Proxy

-- Combined output union
data MenuOutput
  = MenuSignIn Boolean
  | MenuSignInTest String
  | MenuSignOut
  | MenuDrawSettings
  | MenuProfile

menuComponent
  :: forall query m
   . MonadAff m
  => H.Component query (Maybe Message.Player) MenuOutput m
menuComponent = Hooks.component \{ outputToken } maybePlayer -> Hooks.do
  Hooks.pure $ HH.div
    [ HP.id "menu"
    , HCSS.style $ CSS.position $ CSS.relative
    ]
    [ HH.div
        [ HCSS.style $ CSS.cursor CSSCursor.pointer
        , HE.onClick $ const menuOnClick
        , HE.onMouseLeave $ const menuOnMouseLeave
        ] $
        Maybe.maybe
          -- only icon when logged out
          [ HH.fromPlainHTML svgMenu ]
          ( \player ->
              -- show player label + icon when logged in
              [ HH.label
                  [ HCSS.style do
                      CSS.cursor CSSCursor.pointer
                      CSSVerticalAlign.verticalAlign CSSVerticalAlign.Middle
                      CSS.padding (CSS.px 5.0) (CSS.px 10.0) (CSS.px 5.0) (CSS.px 10.0)
                      traverse_ CSS.color $ CSS.fromHexString "#333"
                  ]
                  [ HH.text player.nickname ]
              , HH.fromPlainHTML svgMenu
              ]
          )
          maybePlayer
    , HH.div
        [ HP.id menuListId
        , HCSS.style do
            CSS.position CSS.absolute
            CSS.top $ CSS.pct 100.0
            CSS.right $ CSS.px 0.0
            traverse_ CSS.backgroundColor $ CSS.fromHexString "#fff"
            traverse_ (CSS.border CSS.solid (CSS.px 1.0)) $ CSS.fromHexString "#ddd"
            CSS.padding (CSS.px 10.0) (CSS.px 10.0) (CSS.px 10.0) (CSS.px 10.0)
            CSS.zIndex 1
            CSS.borderRadius (CSS.px 8.0) (CSS.px 8.0) (CSS.px 8.0) (CSS.px 8.0)
            CSS.boxShadow $ (CSS.rgba 0 0 0 0.05) `CSSBox.bsColor` CSSBox.shadowWithBlur (CSS.px 0.0) (CSS.px 4.0) (CSS.px 12.0) NonEmpty.:| []
        ]
        [ if Maybe.isJust maybePlayer then
            -- logged-in menu: Sign out + Drawing settings
            HH.div_
              [ HH.button
                  [ HP.class_ $ wrap "menu-item"
                  , HCSS.style buttonStyle
                  , HE.onClick $ const $ Hooks.raise outputToken MenuSignOut
                  ]
                  [ HH.text "Sign out" ]
              , HH.button
                  [ HP.class_ $ wrap "menu-item"
                  , HCSS.style buttonStyle
                  , HE.onClick $ const $ Hooks.raise outputToken MenuProfile
                  ]
                  [ HH.text "Profile" ]
              , HH.button
                  [ HP.class_ $ wrap "menu-item"
                  , HCSS.style buttonStyle
                  , HE.onClick $ const $ Hooks.raise outputToken MenuDrawSettings
                  ]
                  [ HH.text "Drawing settings" ]
              ]
          else
            -- logged-out menu: Sign in (or test input) + Remember me + Drawing settings
            HH.div_ $
              ( if not testBuild then
                  -- normal sign-in button
                  [ HH.button
                      [ HP.class_ $ wrap "menu-item"
                      , HCSS.style do
                          buttonStyle
                          CSS.width $ CSS.pct 100.0
                      , HE.onClick $ const $
                          let
                            rememebrMeEff =
                              liftEffect $ do
                                w <- HTML.window
                                d <- Window.document w
                                maybeInput <- map (_ >>= HTMLInputElement.fromElement) $ getElementById "remember-me" (Document.toNonElementParentNode d)
                                Maybe.maybe (pure false) HTMLInputElement.checked maybeInput
                          in
                            rememebrMeEff >>= (Hooks.raise outputToken <<< MenuSignIn)
                      ]
                      [ HH.text "Sign in" ]
                  ]
                else
                  -- test build: text input to enter a name
                  [ HH.input
                      [ HP.id "test-name"
                      , HCSS.style do
                          CSS.width $ CSS.pct 100.0
                          CSS.boxSizing $ CSS.fromString "border-box"
                          CSS.marginBottom $ CSS.px 5.0
                      , HE.onKeyDown $ \e -> when (KeyboardEvent.key e == "Enter") do
                          maybeName <- liftEffect do
                            w <- HTML.window
                            d <- Window.document w
                            maybeInput <- map (_ >>= HTMLInputElement.fromElement) $ getElementById "test-name" (Document.toNonElementParentNode d)
                            traverse HTMLInputElement.value maybeInput
                          for_ maybeName \name -> Hooks.raise outputToken $ MenuSignInTest name
                      ]
                  ]
              )
                <>
                  -- remember-me checkbox + label and drawing settings button
                  [ HH.div
                      [ HCSS.style do
                          CSS.width $ CSS.pct 100.0
                          CSS.display CSS.flex
                          CSS.justifyContent CSSCommon.center
                          CSS.padding (CSS.px 5.0) (CSS.px 0.0) (CSS.px 5.0) (CSS.px 0.0)
                      ]
                      [ HH.input
                          [ HP.id "remember-me"
                          , HP.type_ HP.InputCheckbox
                          , HP.checked true
                          , HCSS.style do
                              CSS.margin (CSS.px 0.0) (CSS.px 3.0) (CSS.px 0.0) (CSS.px 0.0)
                              CSS.cursor CSSCursor.pointer
                          ]
                      , HH.label
                          [ HP.for "remember-me"
                          , HCSS.style $ do
                              CSS.fontSize (CSS.rem 0.75)
                              CSS.key (CSS.fromString "white-space") "nowrap"
                              traverse_ CSS.color $ CSS.fromHexString "#333"
                              CSS.cursor CSSCursor.pointer
                          ]
                          [ HH.text "Remember me" ]
                      ]
                  , HH.button
                      [ HP.class_ $ wrap "menu-item"
                      , HCSS.style buttonStyle
                      , HE.onClick $ const $ Hooks.raise outputToken MenuDrawSettings
                      ]
                      [ HH.text "Drawing settings" ]
                  ]
        ]
    ]

_field :: Proxy "field"
_field = Proxy

data AppQuery a = AppQueryMessage Message.Response a | AppQueryHistory AppHistoryState a

type AppInput =
  { playerId :: Maybe Message.PlayerId
  , players :: Message.Players
  , openGames :: Message.OpenGames
  , games :: Message.Games
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
  [ SvgAttributes.id hamburgerId
  , SvgAttributes.width 32.0
  , SvgAttributes.height 32.0
  , HCSS.style $ CSSVerticalAlign.verticalAlign CSSVerticalAlign.Middle
  ]
  let
    line c y =
      [ SvgAttributes.classes [ wrap lineClass, wrap c ]
      , SvgAttributes.x1 4.0
      , SvgAttributes.y1 y
      , SvgAttributes.x2 28.0
      , SvgAttributes.y2 y
      , SvgAttributes.stroke $ SvgAttributes.Named "dimgrey"
      , SvgAttributes.strokeWidth 4.0
      , SvgAttributes.strokeLineCap LineCapRound
      ]
  in
    [ SvgElements.line $ line topClass 8.0
    , SvgElements.line $ line middleClass 16.0
    , SvgElements.line $ line bottomClass 24.0
    ]

data AppHistoryState
  = AppHistoryStateEmpty
  | AppHistoryStateGame Message.GameId
  | AppHistoryStateDrawSettings
  | AppHistoryStateProfile
  | AppHistoryStateLocalGame

putHistoryState :: AppHistoryState -> Effect Unit
putHistoryState state = do
  window <- HTML.window
  location <- Window.location window
  history <- Window.history window
  url <- Location.search location
  currentPath <- Location.pathname location
  let
    newSearchParams =
      ( case state of
          AppHistoryStateGame gameId -> URLSearchParams.set "game" gameId
          _ -> URLSearchParams.delete "game"
      )
        $ URLSearchParams.fromString url
    newStr = URLSearchParams.toString newSearchParams
    newUrl = currentPath <> if newStr == "" then "" else "?" <> newStr
  History.pushState (unsafeToForeign state) (History.DocumentTitle "kropki") (History.URL newUrl) history

data AppState
  = AppStateEmpty
  | AppStateGame
      { gameId :: Message.GameId
      , config :: Message.GameConfig
      , redPlayer :: Message.Player
      , blackPlayer :: Message.Player
      , puttingTime :: Instant.Instant
      , timeLeft :: { red :: Milliseconds, black :: Milliseconds }
      , fields :: NonEmptyList.NonEmptyList Field.Field
      }
  | AppStateDrawSettings
  | AppStateProfile { availability :: Availability }
  | AppStateLocalGame
      { config :: Message.GameConfig
      , puttingTime :: Instant.Instant
      , timeLeft :: { red :: Milliseconds, black :: Milliseconds }
      , fields :: NonEmptyList.NonEmptyList Field.Field
      }

drawSettingsKey :: String
drawSettingsKey = "draw-settings"

renderGameHeader
  :: forall w i
   . String
  -> String
  -> { red :: Milliseconds, black :: Milliseconds }
  -> NonEmptyList.NonEmptyList Field.Field
  -> Array (HH.HTML w i)
renderGameHeader redName blackName timeLeft fields =
  let
    currentField = NonEmptyList.head fields
    nextPlayer = Field.nextPlayer currentField
    redTicking = nextPlayer == Player.Red
    scoreRed = Field.scoreRed currentField
    scoreBlack = Field.scoreBlack currentField
  in
    [ HH.fromPlainHTML $ countdown redTicking timeLeft.red
    , HH.fromPlainHTML $ svgDot "red"
    , HH.div
        [ HCSS.style do
            CSS.display CSS.flex
            CSS.alignItems CSSCommon.center
            CSS.fontSize (CSS.rem 0.9)
        ]
        [ -- Red Player Name
          HH.span_ [ HH.text redName ]
        -- The Score Badge
        , HH.div
            [ HCSS.style do
                traverse_ CSS.backgroundColor $ CSS.fromHexString "#e0e0e0"
                traverse_ CSS.color $ CSS.fromHexString "#333"
                CSS.padding (CSS.px 2.0) (CSS.px 8.0) (CSS.px 2.0) (CSS.px 8.0)
                CSS.marginLeft (CSS.px 8.0)
                CSS.marginRight (CSS.px 8.0)
                CSS.borderRadius (CSS.px 12.0) (CSS.px 12.0) (CSS.px 12.0) (CSS.px 12.0)
                CSS.fontWeight CSS.bold
                CSS.fontFamily [] $ CSSFont.monospace NonEmpty.:| []
            ]
            [ HH.text $ show scoreRed <> " : " <> show scoreBlack ]
        -- Black Player Name
        , HH.span_ [ HH.text blackName ]
        ]
    , HH.fromPlainHTML $ svgDot "black"
    , HH.fromPlainHTML $ countdown (not redTicking) timeLeft.black
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
    drawSettings /\ drawSettingsId <- UsePersistentState.usePersistentState drawSettingsKey defaultDrawSettings
    state /\ stateId <- Hooks.useState AppStateEmpty

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
      unsubscribe = Maybe.maybe (pure unit) (\oldGameId -> Hooks.raise outputToken $ Message.UnsubscribeRequest oldGameId) watchingGameId
      switchToGame gameId = do
        unsubscribe
        Hooks.put watchingGameIdId $ Maybe.Just gameId
        Hooks.raise outputToken $ Message.SubscribeRequest gameId

    Hooks.useQuery queryToken case _ of
      AppQueryHistory historyState a -> do
        case historyState of
          AppHistoryStateEmpty -> unsubscribe *> Hooks.put stateId AppStateEmpty
          AppHistoryStateGame gameId -> switchToGame gameId
          AppHistoryStateDrawSettings -> unsubscribe *> Hooks.put stateId AppStateDrawSettings
          AppHistoryStateProfile -> unsubscribe *> Hooks.put stateId (AppStateProfile { availability: AvailabilityInit })
          AppHistoryStateLocalGame -> unsubscribe *> Hooks.put stateId AppStateEmpty -- TODO: Restore local game
        pure $ Maybe.Just a
      AppQueryMessage response a -> do
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
            if Maybe.Just gameId == watchingGameId then do
              Hooks.put stateId $ AppStateGame
                { gameId
                , config: game.config
                , redPlayer: game.redPlayer
                , blackPlayer: game.blackPlayer
                , puttingTime: initTime
                , timeLeft: { red: Milliseconds $ Int.toNumber timeLeft.red, black: Milliseconds $ Int.toNumber timeLeft.black }
                , fields: Array.foldl
                    ( \fields move ->
                        Maybe.maybe fields (_ `NonEmptyList.cons` fields) $ Field.putPoint (Tuple move.coordinate.x move.coordinate.y) (unwrap move.player) $ NonEmptyList.head fields
                    )
                    (NonEmptyList.singleton $ Field.emptyField game.config.size.width game.config.size.height)
                    moves
                }
              liftEffect $ putHistoryState $ AppHistoryStateGame gameId
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
            case state of
              AppStateGame game | game.gameId == gameId ->
                Hooks.put stateId $ AppStateGame game
                  { puttingTime = puttingTime
                  , timeLeft = { red: Milliseconds $ Int.toNumber timeLeft.red, black: Milliseconds $ Int.toNumber timeLeft.black }
                  , fields = Maybe.maybe game.fields (_ `NonEmptyList.cons` game.fields)
                      $ Field.putPoint (Tuple move.coordinate.x move.coordinate.y) (unwrap move.player)
                      $ NonEmptyList.head game.fields
                  }
              _ ->
                liftEffect $ Console.warn $ "Wrong game to put point"
          Message.DrawResponse _ _ ->
            pure unit
          Message.GameResultResponse _ _ ->
            pure unit
          Message.NicknameChangedResponse playerId player ->
            Hooks.modify_ playersId $ Map.insert playerId player
          Message.NicknameAvailableResponse nickname available ->
            case state of
              AppStateProfile { availability: AvailabilityChecking nickname' } | nickname == nickname' ->
                Hooks.put stateId $ AppStateProfile
                  { availability: (if available then AvailabilityAvailable else AvailabilityTaken) nickname }
              _ -> pure unit
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
                    unsubscribe
                    Hooks.put watchingGameIdId Maybe.Nothing
                    Hooks.put stateId AppStateEmpty
                    liftEffect $ putHistoryState AppHistoryStateEmpty
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
                ] $ case state of
                AppStateGame game ->
                  renderGameHeader
                    game.redPlayer.nickname
                    game.blackPlayer.nickname
                    game.timeLeft
                    game.fields
                AppStateLocalGame game ->
                  renderGameHeader
                    "Player 1"
                    "Player 2"
                    game.timeLeft
                    game.fields
                _ -> []
            , HH.div
                [ HCSS.style $ CSS.marginLeft CSSCommon.auto
                ]
                [ HH.slot
                    _menu
                    unit
                    menuComponent
                    (activePlayerId >>= (flip Map.lookup players))
                    case _ of
                      MenuSignIn rememberMe ->
                        Hooks.raise outputToken $ Message.GetAuthUrlRequest rememberMe
                      MenuSignInTest name ->
                        Hooks.raise outputToken $ Message.AuthTestRequest name
                      MenuSignOut -> do
                        Hooks.raise outputToken Message.SignOutRequest
                        Hooks.put activePlayerIdId Maybe.Nothing
                        liftEffect $ setCookie "kropki=; Path=/; Expires=Thu, 01 Jan 1970 00:00:01 GMT;"
                      MenuDrawSettings -> do
                        unsubscribe
                        Hooks.put watchingGameIdId Maybe.Nothing
                        Hooks.put stateId AppStateDrawSettings
                        liftEffect $ putHistoryState AppHistoryStateDrawSettings
                      MenuProfile -> do
                        unsubscribe
                        Hooks.put watchingGameIdId Maybe.Nothing
                        Hooks.put stateId $ AppStateProfile { availability: AvailabilityInit }
                        liftEffect $ putHistoryState AppHistoryStateProfile
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
                    CSS.fontFamily [] $ CSSFont.sansSerif NonEmpty.:| []
                    traverse_ CSS.backgroundColor $ CSS.fromHexString "#f9f9f9"
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
                [ case state of
                    AppStateGame game ->
                      let
                        game' = Map.lookup game.gameId games
                        nextPlayer = Field.nextPlayer $ NonEmptyList.head game.fields
                        pointer = Maybe.isJust activePlayerId &&
                          ( map _.redPlayerId game' == activePlayerId && nextPlayer == Player.Red ||
                              map _.blackPlayerId game' == activePlayerId && nextPlayer == Player.Black
                          )
                      in
                        HH.slot
                          _field
                          unit
                          fieldComponent
                          { fields: game.fields, pointer, drawSettings }
                          \(Click (Tuple x y)) -> when pointer do
                            now' <- liftEffect $ Now.now
                            let
                              elapsed :: Milliseconds
                              elapsed = Instant.diff now' game.puttingTime
                              diff = unwrap elapsed
                            Hooks.put stateId $ AppStateGame game
                              { puttingTime = now'
                              , timeLeft = case nextPlayer of
                                  Player.Red -> { red: Milliseconds $ max 0.0 (unwrap game.timeLeft.red - diff), black: game.timeLeft.black }
                                  Player.Black -> { red: game.timeLeft.red, black: Milliseconds $ max 0.0 (unwrap game.timeLeft.black - diff) }
                              , fields = Maybe.maybe game.fields (_ `NonEmptyList.cons` game.fields)
                                  $ Field.putPoint (Tuple x y) nextPlayer
                                  $ NonEmptyList.head game.fields
                              }
                            Hooks.raise outputToken $ Message.PutPointRequest game.gameId { x, y }
                    AppStateLocalGame game ->
                      let
                        nextPlayer = Field.nextPlayer $ NonEmptyList.head game.fields
                      in
                        HH.slot
                          _field
                          unit
                          fieldComponent
                          { fields: game.fields, pointer: true, drawSettings }
                          \(Click (Tuple x y)) -> do
                            now' <- liftEffect $ Now.now
                            let
                              elapsed :: Milliseconds
                              elapsed = Instant.diff now' game.puttingTime
                              diff = unwrap elapsed
                              increment = Int.toNumber game.config.time.increment * 1000.0
                            Hooks.put stateId $ AppStateLocalGame game
                              { puttingTime = now'
                              , timeLeft = case nextPlayer of
                                  Player.Red -> { red: Milliseconds $ max 0.0 (unwrap game.timeLeft.red - diff) + increment, black: game.timeLeft.black }
                                  Player.Black -> { red: game.timeLeft.red, black: Milliseconds $ max 0.0 (unwrap game.timeLeft.black - diff) + increment }
                              , fields = Maybe.maybe game.fields (_ `NonEmptyList.cons` game.fields)
                                  $ Field.putPoint (Tuple x y) nextPlayer
                                  $ NonEmptyList.head game.fields
                              }
                    AppStateDrawSettings ->
                      HH.slot
                        _drawSettings
                        unit
                        drawSettingsComponent
                        drawSettings
                        case _ of
                          UpdateSettings settings -> do
                            UsePersistentState.put drawSettingsKey drawSettingsId settings
                            Hooks.put stateId AppStateEmpty
                            liftEffect $ putHistoryState AppHistoryStateEmpty
                    AppStateProfile { availability } ->
                      case activePlayerId >>= flip Map.lookup players of
                        Maybe.Nothing -> HH.div_ []
                        Maybe.Just player ->
                          HH.slot
                            _profileSettings
                            unit
                            profileSettingsComponent
                            { currentNickname: player.nickname, availability }
                            case _ of
                              CheckNickname nickname ->
                                if nickname == "" || nickname == player.nickname then
                                  Hooks.put stateId $ AppStateProfile { availability: AvailabilityInit }
                                else do
                                  Hooks.put stateId $ AppStateProfile { availability: AvailabilityChecking nickname }
                                  Hooks.raise outputToken $ Message.CheckNicknameRequest nickname
                              ChangeNickname nickname -> do
                                Hooks.raise outputToken $ Message.ChangeNicknameRequest nickname
                                Hooks.put stateId AppStateEmpty
                                liftEffect $ putHistoryState AppHistoryStateEmpty
                    AppStateEmpty ->
                      HH.slot
                        _createGame
                        unit
                        createGameComponent
                        (activePlayerId /\ openGames)
                        case _ of
                          CreateGame config -> Hooks.raise outputToken $ Message.CreateRequest config
                          CreateLocalGame config -> do
                            now <- liftEffect $ Now.now
                            Hooks.put stateId $ AppStateLocalGame
                              { config
                              , puttingTime: now
                              , timeLeft: { red: Milliseconds $ Int.toNumber $ config.time.total * 1000, black: Milliseconds $ Int.toNumber $ config.time.total * 1000 }
                              , fields: NonEmptyList.singleton $ Field.emptyField config.size.width config.size.height
                              }
                            liftEffect $ putHistoryState AppHistoryStateLocalGame
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

type ChildMessage = { code :: String, state :: String }

readChildMessage :: Foreign -> Except (NonEmptyList.NonEmptyList ForeignError) ChildMessage
readChildMessage value = do
  code <- value ! "code" >>= readString
  state <- value ! "state" >>= readString
  pure { code, state }

rosterItemRowClass :: String
rosterItemRowClass = "roster-item-row"

clickableClass :: String
clickableClass = "clickable"

rosterNameClass :: String
rosterNameClass = "roster-name"

rosterMetaClass :: String
rosterMetaClass = "roster-meta"

rosterHeaderClass :: String
rosterHeaderClass = "roster-header"

menuListId :: String
menuListId = "menu-list"

hamburgerId :: String
hamburgerId = "hamburger"

lineClass :: String
lineClass = "line"

topClass :: String
topClass = "top"

middleClass :: String
middleClass = "middle"

bottomClass :: String
bottomClass = "bottom"

main :: Effect Unit
main = do
  window <- HTML.window
  redirect <- checkRedirect window
  unless redirect do
    let connectionEffect = WS.create (if testBuild then "ws://127.0.0.1:8080" else "wss://kropki.org/ws") []
    connection <- connectionEffect
    connectionRef <- Ref.new connection

    listener <- EET.eventListener \event ->
      for_ (runExcept $ readChildMessage $ eventData event) $ \message ->
        wsSender connectionRef $ Message.AuthRequest message.code message.state
    EET.addEventListener (wrap "message") listener true $ Window.toEventTarget window

    location <- Window.location window
    currentPath <- Location.pathname location
    history <- Window.history window
    History.replaceState (unsafeToForeign AppHistoryStateEmpty) (History.DocumentTitle "kropki") (History.URL currentPath) history

    HA.runHalogenAff do
      body <- HA.awaitBody
      CR.runProcess $ wsProducer connectionRef connectionEffect CR.$$ do
        input <-
          let
            read = CR.await >>= case _ of
              Message.InitResponse playerId players openGames games ->
                pure { playerId, players, openGames, games }
              other -> (Console.warn $ "Unexpected first message: " <> show other) *> read
          in
            read
        io <- lift $ runUI appComponent input body

        _ <- H.liftEffect $ HS.subscribe io.messages $ wsSender connectionRef

        popStateListener <- H.liftEffect $ EET.eventListener \ev ->
          for_ (PopStateEvent.fromEvent ev) \pse -> do
            let foreignState = PopStateEvent.state pse
            unless (isNull foreignState || isUndefined foreignState) do
              let state = unsafeFromForeign foreignState
              Aff.launchAff_ $ void $ io.query $ H.mkTell $ AppQueryHistory state
        H.liftEffect $ EET.addEventListener (wrap "popstate") popStateListener false (Window.toEventTarget window)

        CR.consumer \response -> (io.query $ H.mkTell $ AppQueryMessage response) *> pure Maybe.Nothing
