module Main where

import Prelude

import CSS as CSS
import Control.Coroutine as CR
import Control.Coroutine.Aff (emit)
import Control.Coroutine.Aff as CRA
import Control.Monad.Except (runExcept)
import Control.Monad.Maybe.Trans (MaybeT(..), mapMaybeT, runMaybeT)
import Control.Monad.Trans.Class (lift)
import Countdown (countdown)
import DOM.HTML.Indexed.StepValue (StepValue(..))
import Data.Argonaut (decodeJson, encodeJson, parseJson, stringify)
import Data.Array as Array
import Data.DateTime.Instant as Instant
import Data.Either as Either
import Data.Foldable (for_, traverse_)
import Data.Int as Int
import Data.List (List(..))
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Newtype (unwrap, wrap)
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
import Foreign (readString, unsafeToForeign, unsafeFromForeign, isNull, isUndefined)
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
import SidePanelComponent as SidePanelComponent
import Sgf as Sgf
import Type.Proxy (Proxy(..))
import UsePersistentState as UsePersistentState
import Web.DOM.DOMTokenList as DOMTokenList
import Web.DOM.Element as Element
import Web.DOM.MutationObserver as MutationObserver
import Web.DOM.NonElementParentNode (getElementById)
import Web.Event.Event as Event
import Web.Event.EventTarget as EET
import Web.HTML as HTML
import Web.HTML.Event.PopStateEvent as PopStateEvent
import Web.HTML.HTMLDocument as Document
import Web.HTML.HTMLInputElement as HTMLInputElement
import Web.HTML.History as History
import Web.HTML.Location as Location
import Web.HTML.Window as Window
import Web.Socket.Event.EventTypes as WSET
import Web.Socket.Event.MessageEvent as ME
import Web.Socket.WebSocket as WS
import Web.Storage.Storage (getItem, removeItem, setItem)
import Web.UIEvent.KeyboardEvent as KeyboardEvent
import Web.URL.URLSearchParams as URLSearchParams

testBuild :: Boolean
testBuild = false

foreign import setCookie :: String -> Effect Unit
foreign import saveFile :: String -> String -> Effect Unit
foreign import playMoveSound :: Effect Unit

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

buttonClass :: String
buttonClass = "btn"

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
          [ HP.class_ $ wrap sectionHeaderClass ]
          [ HH.text title ]

      isDirty = nickname /= input.currentNickname
      isValid = isDirty && input.availability == AvailabilityAvailable nickname

    Hooks.pure $ HH.div
      [ HP.class_ $ wrap overlayContainerClass ]
      [ HH.div
          [ HP.class_ $ wrap cardPanelClass ]
          [ HH.div
              [ HCSS.style $ CSS.marginBottom (CSS.px 30.0) ]
              [ renderHeader "Profile Settings"
              , HH.table_
                  [ HH.tr_
                      [ HH.td
                          [ HP.class_ $ wrap tdLabelClass ]
                          [ HH.text "Nickname" ]
                      , HH.td
                          [ HP.class_ $ wrap tdValueClass ]
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
                              , HP.classes [ wrap inputNicknameClass ]
                              , HCSS.style do
                                  traverse_ (CSS.border CSS.solid (CSS.px 2.0)) borderColor
                              ]
                          ]
                      ]
                  ]
              ]
          , HH.div
              [ HP.class_ $ wrap btnGroupClass ]
              [ HH.button
                  [ HP.disabled (not isValid)
                  , HP.classes [ wrap btnPrimaryClass, wrap btnMrClass, wrap (if isValid then btnPointerClass else btnNotAllowedClass) ]
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
      -- Helper to render a generic row (Number/Int/String/Color)
      renderInputRow label inputType val minVal maxVal stepVal onChange =
        HH.tr_
          [ HH.td
              [ HP.class_ $ wrap tdLabelClass ]
              [ HH.text label ]
          , HH.td
              [ HP.class_ $ wrap tdValueClass ]
              [ HH.input $
                  [ HP.type_ inputType
                  , HP.value val
                  , HE.onValueInput onChange
                  , HP.class_ $ wrap inputSmallClass
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
              [ HP.class_ $ wrap tdLabelClass ]
              [ HH.text label ]
          , HH.td
              [ HP.class_ $ wrap tdValueClass ]
              [ HH.input
                  [ HP.type_ HP.InputCheckbox
                  , HP.checked isChecked
                  , HE.onChecked \_ -> onToggle (not isChecked)
                  , HP.class_ $ wrap inputCheckboxClass
                  ]
              ]
          ]

      -- Render Section Header
      renderHeader title =
        HH.h3
          [ HP.class_ $ wrap sectionHeaderClass ]
          [ HH.text title ]

    Hooks.pure $ HH.div
      [ HP.class_ $ wrap overlayContainerClass ]
      [ HH.div
          [ HP.class_ $ wrap cardPanelClass ]
          [ -- Main Layout: Settings inputs (left) + Preview (right)
            HH.div
              [ HP.class_ $ wrap settingsRowClass ]
              [ -- LEFT COLUMN: Settings Inputs
                HH.div
                  [ HP.class_ $ wrap settingsColumnsClass ]
                  [ -- Sub-column 1: Appearance & Options
                    HH.div
                      [ HP.class_ $ wrap settingsColLeftClass ]
                      [ renderHeader "Appearance"
                      , HH.table_
                          [ renderInputRow "Grid Thickness" HP.InputNumber (show settings.gridThickness) (Maybe.Just 1.0) (Maybe.Just 5.0) (Maybe.Just $ Step 1.0)
                              \s -> traverse_ (\v -> Hooks.modify_ settingsId _ { gridThickness = v }) (Int.fromString s)
                          , renderInputRow "Point Radius" HP.InputNumber (show settings.pointRadius) (Maybe.Just 0.1) (Maybe.Just 5.0) (Maybe.Just $ Step 0.1)
                              \s -> traverse_ (\v -> Hooks.modify_ settingsId _ { pointRadius = v }) (Number.fromString s)
                          , renderInputRow "Opacity" HP.InputNumber (show settings.fillingAlpha) (Maybe.Just 0.0) (Maybe.Just 1.0) (Maybe.Just $ Step 0.1)
                              \s -> traverse_ (\v -> Hooks.modify_ settingsId _ { fillingAlpha = v }) (Number.fromString s)
                          ]
                      , HH.div [ HP.class_ $ wrap sectionSpacerClass ] []
                      , renderHeader "Options"
                      , HH.table_
                          [ renderCheckboxRow "Full Fill" settings.fullFill (\v -> Hooks.modify_ settingsId _ { fullFill = v })
                          , renderCheckboxRow "Extended Fill" settings.extendedFill (\v -> Hooks.modify_ settingsId _ { extendedFill = v })
                          , renderCheckboxRow "Inner Surroundings" settings.innerSurroundings (\v -> Hooks.modify_ settingsId _ { innerSurroundings = v })
                          , renderCheckboxRow "H-Reflection" settings.hReflection (\v -> Hooks.modify_ settingsId _ { hReflection = v })
                          , renderCheckboxRow "V-Reflection" settings.vReflection (\v -> Hooks.modify_ settingsId _ { vReflection = v })
                          ]
                      ]
                  , -- Sub-column 2: Colors & Coordinates
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
                      , HH.div [ HP.class_ $ wrap sectionSpacerClass ] []
                      , renderHeader "Coordinates"
                      , HH.table_
                          [ renderCheckboxRow "Top" settings.coordsTop (\v -> Hooks.modify_ settingsId _ { coordsTop = v })
                          , renderCheckboxRow "Right" settings.coordsRight (\v -> Hooks.modify_ settingsId _ { coordsRight = v })
                          , renderCheckboxRow "Bottom" settings.coordsBottom (\v -> Hooks.modify_ settingsId _ { coordsBottom = v })
                          , renderCheckboxRow "Left" settings.coordsLeft (\v -> Hooks.modify_ settingsId _ { coordsLeft = v })
                          ]
                      ]
                  ]
              , -- RIGHT COLUMN: Live Preview
                HH.div
                  [ HP.class_ $ wrap previewColClass ]
                  [ renderHeader "Preview"
                  , HH.div
                      [ HP.class_ $ wrap previewBoxClass ]
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
              [ HP.class_ $ wrap btnGroupClass ]
              [ HH.button
                  [ HP.classes [ wrap btnPrimaryClass, wrap btnMrClass, wrap btnPointerClass ]
                  , HE.onClick $ const $ Hooks.raise outputToken $ UpdateSettings settings
                  ]
                  [ HH.text "Update Settings" ]
              , HH.button
                  [ HP.classes [ wrap btnPrimaryClass, wrap btnPointerClass ]
                  , HE.onClick $ const $ Hooks.put settingsId defaultDrawSettings
                  ]
                  [ HH.text "Reset to Default" ]
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
    opening /\ openingId <- Hooks.useState Message.Cross
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
            , opening: openGame.config.opening
            , disabled: true
            }
        Maybe.Nothing ->
          { width: "39"
          , height: "32"
          , minutes: "5"
          , seconds: "0"
          , increment: "5"
          , opening: opening
          , disabled: false
          }

      -- Render Helper for Table Rows
      renderRow id label value minVal maxVal isDisabled =
        HH.tr_
          [ HH.td
              [ HP.class_ $ wrap tdLabelClass ]
              [ HH.text label ]
          , HH.td
              [ HP.class_ $ wrap tdValueClass ]
              [ HH.input
                  [ HP.id id
                  , HP.type_ HP.InputNumber
                  , HP.value value
                  , HP.min minVal
                  , HP.max maxVal
                  , HP.disabled isDisabled
                  , HP.class_ $ wrap inputCreateClass
                  ]
              ]
          ]

      -- Render Section Header
      renderHeader title =
        HH.h3
          [ HP.class_ $ wrap sectionHeaderClass ]
          [ HH.text title ]

    Hooks.pure $ HH.div
      [ HP.class_ $ wrap overlayContainerClass ]
      [ HH.div
          [ HP.class_ $ wrap cardPanelClass ]
          [ -- Config Grid
            HH.div
              [ HP.class_ $ wrap settingsColumnsClass ]
              [ -- Column 1: Board Size
                HH.div
                  [ HP.class_ $ wrap settingsColLeftClass ]
                  [ renderHeader "Board Size"
                  , HH.table_
                      [ renderRow "width" "Width" displayState.width 10.0 50.0 displayState.disabled
                      , renderRow "height" "Height" displayState.height 10.0 50.0 displayState.disabled
                      , HH.tr_
                          [ HH.td
                              [ HP.class_ $ wrap tdLabelClass ]
                              [ HH.text "Opening" ]
                          , HH.td
                              [ HP.class_ $ wrap tdValueClass ]
                              [ HH.select
                                  [ HP.disabled displayState.disabled
                                  , HE.onValueChange \val -> case val of
                                      "Cross" -> Hooks.put openingId Message.Cross
                                      "TwoCrosses" -> Hooks.put openingId Message.TwoCrosses
                                      "TripleCross" -> Hooks.put openingId Message.TripleCross
                                      _ -> pure unit
                                  ]
                                  [ HH.option [ HP.value "Cross", HP.selected (displayState.opening == Message.Cross) ] [ HH.text "Cross" ]
                                  , HH.option [ HP.value "TwoCrosses", HP.selected (displayState.opening == Message.TwoCrosses) ] [ HH.text "Two Crosses" ]
                                  , HH.option [ HP.value "TripleCross", HP.selected (displayState.opening == Message.TripleCross) ] [ HH.text "Triple Cross" ]
                                  ]
                              ]
                          ]
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
              [ HP.class_ $ wrap btnGroupClass ]
              case existingGame of
                Maybe.Nothing ->
                  [ HH.button
                      [ HP.disabled $ Maybe.isNothing activePlayerId
                      , HP.title $ if Maybe.isNothing activePlayerId then "You must be signed in to create a game" else ""
                      , HP.classes [ wrap btnPrimaryClass, wrap btnMrClass, wrap (if Maybe.isNothing activePlayerId then btnNotAllowedClass else btnPointerClass) ]
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
                              , opening
                              }
                      ]
                      [ HH.text "Create game" ]
                  , HH.button
                      [ HP.classes [ wrap btnPrimaryClass, wrap btnPointerClass ]
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
                              , opening
                              }
                      ]
                      [ HH.text "Local game" ]
                  ]
                Maybe.Just (Tuple gameId _) ->
                  [ HH.button
                      [ HP.classes [ wrap btnPrimaryClass, wrap btnPointerClass ]
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
    contains <- lift $ DOMTokenList.contains classes "open"
    if contains then
      lift $ DOMTokenList.remove classes "open"
    else
      lift $ DOMTokenList.add classes "open"

closeMenu :: forall e. MonadEffect e => e Unit
closeMenu = liftEffect $ do
  window <- HTML.window
  document <- Window.document window
  void $ runMaybeT $ do
    menu <- wrap $ getElementById "menu" (Document.toNonElementParentNode document)
    classes <- lift $ Element.classList menu
    lift $ DOMTokenList.remove classes "open"

_menu :: Proxy "menu"
_menu = Proxy

-- Combined output union
data MenuOutput
  = MenuSignIn Boolean
  | MenuSignInTest String
  | MenuSignOut
  | MenuDrawSettings
  | MenuProfile
  | MenuSaveSgf

menuComponent
  :: forall query m
   . MonadAff m
  => H.Component query (Maybe Message.Player) MenuOutput m
menuComponent = Hooks.component \{ outputToken } maybePlayer -> Hooks.do
  Hooks.pure $ HH.div
    [ HP.id "menu"
    , HP.class_ $ wrap menuPosClass
    ]
    [ HH.div
        [ HP.class_ $ wrap menuTriggerClass
        , HE.onClick $ const menuOnClick
        ] $
        Maybe.maybe
          -- only icon when logged out
          [ HH.fromPlainHTML svgMenu ]
          ( \player ->
              -- show player label + icon when logged in
              [ HH.label
                  [ HP.class_ $ wrap navLabelClass ]
                  [ HH.text player.nickname ]
              , HH.fromPlainHTML svgMenu
              ]
          )
          maybePlayer
    , HH.div
        [ HP.id menuListId
        , HP.class_ $ wrap menuDropdownClass
        ]
        [ if Maybe.isJust maybePlayer then
            -- logged-in menu: Sign out + Drawing settings
            HH.div_
              [ HH.button
                  [ HP.classes [ wrap "menu-item", wrap buttonClass ]
                  , HE.onClick $ const $ closeMenu *> Hooks.raise outputToken MenuSignOut
                  ]
                  [ HH.text "Sign out" ]
              , HH.button
                  [ HP.classes [ wrap "menu-item", wrap buttonClass ]
                  , HE.onClick $ const $ closeMenu *> Hooks.raise outputToken MenuProfile
                  ]
                  [ HH.text "Profile" ]
              , HH.button
                  [ HP.classes [ wrap "menu-item", wrap buttonClass ]
                  , HE.onClick $ const $ closeMenu *> Hooks.raise outputToken MenuDrawSettings
                  ]
                  [ HH.text "Drawing settings" ]
              , HH.button
                  [ HP.classes [ wrap "menu-item", wrap buttonClass ]
                  , HE.onClick $ const $ closeMenu *> Hooks.raise outputToken MenuSaveSgf
                  ]
                  [ HH.text "Save SGF" ]
              ]
          else
            -- logged-out menu: Sign in (or test input) + Remember me + Drawing settings
            HH.div_ $
              ( if not testBuild then
                  -- normal sign-in button
                  [ HH.button
                      [ HP.classes [ wrap "menu-item", wrap buttonClass ]
                      , HE.onClick $ const $
                          let
                            rememebrMeEff =
                              liftEffect $ do
                                w <- HTML.window
                                d <- Window.document w
                                maybeInput <- map (_ >>= HTMLInputElement.fromElement) $ getElementById "remember-me" (Document.toNonElementParentNode d)
                                Maybe.maybe (pure false) HTMLInputElement.checked maybeInput
                          in
                            closeMenu *> (rememebrMeEff >>= (Hooks.raise outputToken <<< MenuSignIn))
                      ]
                      [ HH.text "Sign in" ]
                  ]
                else
                  -- test build: text input to enter a name
                  [ HH.input
                      [ HP.id "test-name"
                      , HP.class_ $ wrap testNameInputClass
                      , HE.onKeyDown $ \e -> when (KeyboardEvent.key e == "Enter") do
                          maybeName <- liftEffect do
                            w <- HTML.window
                            d <- Window.document w
                            maybeInput <- map (_ >>= HTMLInputElement.fromElement) $ getElementById "test-name" (Document.toNonElementParentNode d)
                            traverse HTMLInputElement.value maybeInput
                          for_ maybeName \name -> closeMenu *> Hooks.raise outputToken (MenuSignInTest name)
                      ]
                  ]
              )
                <>
                  -- remember-me checkbox + label and drawing settings button
                  [ HH.div
                      [ HP.class_ $ wrap rememberMeRowClass ]
                      [ HH.input
                          [ HP.id "remember-me"
                          , HP.type_ HP.InputCheckbox
                          , HP.checked true
                          , HP.class_ $ wrap rememberMeCheckboxClass
                          ]
                      , HH.label
                          [ HP.for "remember-me"
                          , HP.class_ $ wrap rememberMeLabelClass
                          ]
                          [ HH.text "Remember me" ]
                      ]
                  , HH.button
                      [ HP.classes [ wrap "menu-item", wrap buttonClass ]
                      , HE.onClick $ const $ closeMenu *> Hooks.raise outputToken MenuDrawSettings
                      ]
                      [ HH.text "Drawing settings" ]
                  , HH.button
                      [ HP.classes [ wrap "menu-item", wrap buttonClass ]
                      , HE.onClick $ const $ closeMenu *> Hooks.raise outputToken MenuSaveSgf
                      ]
                      [ HH.text "Save SGF" ]
                  ]
        ]
    ]

_sidePanel :: Proxy "sidePanel"
_sidePanel = Proxy

_field :: Proxy "field"
_field = Proxy

data AppQuery a = AppQueryMessage Message.Response a | AppQueryHistory AppHistoryState a

type AppInput =
  { playerId :: Maybe Message.PlayerId
  , players :: Message.Players
  , openGames :: Message.OpenGames
  , games :: Message.Games
  , watchingGameId :: Maybe Message.GameId
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
  , HP.class_ $ wrap svgValignMiddleClass
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
      , result :: Maybe Message.GameResult
      , drawOffer :: Maybe Message.Color
      }
  | AppStateDrawSettings
  | AppStateProfile { availability :: Availability }
  | AppStateLocalGame
      { config :: Message.GameConfig
      , puttingTime :: Instant.Instant
      , timeLeft :: { red :: Milliseconds, black :: Milliseconds }
      , fields :: NonEmptyList.NonEmptyList Field.Field
      , redoFields :: List.List Field.Field
      }

drawSettingsKey :: String
drawSettingsKey = "draw-settings"

renderGameHeader
  :: forall w i
   . String
  -> String
  -> String
  -> { red :: Milliseconds, black :: Milliseconds }
  -> Boolean
  -> Maybe.Maybe Player.Player
  -> NonEmptyList.NonEmptyList Field.Field
  -> Array (HH.HTML w i)
renderGameHeader gameId redName blackName timeLeft finished winner fields =
  let
    currentField = NonEmptyList.head fields
    nextPlayer = Field.nextPlayer currentField
    redTicking = nextPlayer == Player.Red
    scoreRed = Field.scoreRed currentField
    scoreBlack = Field.scoreBlack currentField
    nameEl player name = case winner of
      Maybe.Just w | w == player -> HH.strong_ [ HH.text name ]
      Maybe.Nothing | not finished -> HH.span_ [ HH.text name ]
      _ -> HH.em_ [ HH.text name ]
  in
    [ HH.fromPlainHTML $ countdown (gameId <> "-red") (not finished && redTicking) timeLeft.red
    , HH.fromPlainHTML $ svgDot "red"
    , HH.div
        [ HP.class_ $ wrap scoreBadgeClass ]
        [ -- Red Player Name
          nameEl Player.Red redName
        -- The Score Badge
        , HH.div
            [ HP.class_ $ wrap scoreValueClass ]
            [ HH.text $ show scoreRed <> " : " <> show scoreBlack ]
        -- Black Player Name
        , nameEl Player.Black blackName
        ]
    , HH.fromPlainHTML $ svgDot "black"
    , HH.fromPlainHTML $ countdown (gameId <> "-black") (not finished && not redTicking) timeLeft.black
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
    watchingGameId /\ watchingGameIdId <- Hooks.useState input.watchingGameId
    drawSettings /\ drawSettingsId <- UsePersistentState.usePersistentState drawSettingsKey defaultDrawSettings
    state /\ stateId <- Hooks.useState AppStateEmpty
    resignPending /\ resignPendingId <- Hooks.useState false
    _ /\ resignTimerSubRef <- Hooks.useRef (Maybe.Nothing :: Maybe H.SubscriptionId)

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

    Hooks.useLifecycleEffect do
      let
        keyEmitter = HS.makeEmitter \push -> do
          window <- HTML.window
          listener <- EET.eventListener \event ->
            for_ (KeyboardEvent.fromEvent event) \ke ->
              case KeyboardEvent.key ke of
                "ArrowLeft" -> push do
                  currentState <- Hooks.get stateId
                  case currentState of
                    AppStateLocalGame game ->
                      case NonEmptyList.uncons game.fields of
                        { head: currentField, tail: Cons prevField rest } -> do
                          now' <- liftEffect $ Now.now
                          let
                            increment = Int.toNumber game.config.time.increment * 1000.0
                            movedPlayer = Field.nextPlayer prevField
                          Hooks.put stateId $ AppStateLocalGame game
                            { puttingTime = now'
                            , fields = NonEmptyList.cons' prevField rest
                            , redoFields = Cons currentField game.redoFields
                            , timeLeft = case movedPlayer of
                                Player.Red -> game.timeLeft { red = Milliseconds $ max 0.0 (unwrap game.timeLeft.red - increment) }
                                Player.Black -> game.timeLeft { black = Milliseconds $ max 0.0 (unwrap game.timeLeft.black - increment) }
                            }
                        _ -> pure unit
                    _ -> pure unit
                "ArrowRight" -> push do
                  currentState <- Hooks.get stateId
                  case currentState of
                    AppStateLocalGame game ->
                      case game.redoFields of
                        Cons redoField rest -> do
                          now' <- liftEffect $ Now.now
                          let
                            elapsed :: Milliseconds
                            elapsed = Instant.diff now' game.puttingTime
                            diff = unwrap elapsed
                            increment = Int.toNumber game.config.time.increment * 1000.0
                            movedPlayer = Field.nextPlayer $ NonEmptyList.head game.fields
                          Hooks.put stateId $ AppStateLocalGame game
                            { puttingTime = now'
                            , fields = NonEmptyList.cons redoField game.fields
                            , redoFields = rest
                            , timeLeft = case movedPlayer of
                                Player.Red -> { red: Milliseconds $ max 0.0 (unwrap game.timeLeft.red - diff) + increment, black: game.timeLeft.black }
                                Player.Black -> { red: game.timeLeft.red, black: Milliseconds $ max 0.0 (unwrap game.timeLeft.black - diff) + increment }
                            }
                        Nil -> pure unit
                    _ -> pure unit
                _ -> pure unit
          EET.addEventListener (wrap "keydown") listener false (Window.toEventTarget window)
          pure $ EET.removeEventListener (wrap "keydown") listener false (Window.toEventTarget window)
        saveEmitter = HS.makeEmitter \push -> do
          window <- HTML.window
          listener <- EET.eventListener \event ->
            for_ (KeyboardEvent.fromEvent event) \ke ->
              when (KeyboardEvent.key ke == "s" && KeyboardEvent.ctrlKey ke) do
                Event.preventDefault event
                push do
                  currentState <- Hooks.get stateId
                  let
                    maybeFields = case currentState of
                      AppStateGame game -> Maybe.Just game.fields
                      AppStateLocalGame game -> Maybe.Just game.fields
                      _ -> Maybe.Nothing
                  for_ maybeFields \fields ->
                    liftEffect $ saveFile "game.sgf" $ Sgf.fieldsToSgf fields
          EET.addEventListener (wrap "keydown") listener false (Window.toEventTarget window)
          pure $ EET.removeEventListener (wrap "keydown") listener false (Window.toEventTarget window)
      keySubscriptionId <- Hooks.subscribe keyEmitter
      saveSubscriptionId <- Hooks.subscribe saveEmitter
      pure $ Maybe.Just $ Hooks.unsubscribe keySubscriptionId *> Hooks.unsubscribe saveSubscriptionId

    let
      cancelResignTimer = do
        maybeSub <- liftEffect $ Ref.read resignTimerSubRef
        for_ maybeSub \subId -> do
          Hooks.unsubscribe subId
          liftEffect $ Ref.write Maybe.Nothing resignTimerSubRef

      handleResignClick gameId = do
        pending <- Hooks.get resignPendingId
        if pending then do
          cancelResignTimer
          Hooks.put resignPendingId false
          Hooks.raise outputToken $ Message.ResignRequest gameId
        else do
          Hooks.put resignPendingId true
          let
            emitter = HS.makeEmitter \push -> do
              fiber <- Aff.launchAff do
                Aff.delay $ Milliseconds 3000.0
                liftEffect $ push $ do
                  Hooks.put resignPendingId false
                  liftEffect $ Ref.write Maybe.Nothing resignTimerSubRef
              pure $ Aff.launchAff_ $ Aff.killFiber (Exception.error "canceled") fiber
          subId <- Hooks.subscribe emitter
          liftEffect $ Ref.write (Maybe.Just subId) resignTimerSubRef

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
          Message.GameInitResponse gameId game moves initTime drawOffer timeLeft result ->
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
                , result
                , drawOffer
                }
              liftEffect $ putHistoryState $ AppHistoryStateGame gameId
            else
              liftEffect $ Console.warn $ "Unexpected init game id " <> gameId
          Message.AuthUrlResponse url authCookie -> liftEffect $ do
            window <- HTML.window
            storage <- Window.localStorage window
            setItem "kropki_auth" authCookie storage
            location <- Window.location window
            Location.assign url location
          Message.AuthResponse playerId cookie -> do
            Hooks.put activePlayerIdId $ Maybe.Just playerId
            liftEffect $ setCookie cookie
            window <- liftEffect $ HTML.window
            storage <- liftEffect $ Window.localStorage window
            liftEffect $ removeItem "kropki_auth" storage
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
              AppStateGame game | game.gameId == gameId -> do
                liftEffect playMoveSound
                Hooks.put stateId $ AppStateGame game
                  { puttingTime = puttingTime
                  , timeLeft = { red: Milliseconds $ Int.toNumber timeLeft.red, black: Milliseconds $ Int.toNumber timeLeft.black }
                  , drawOffer = Maybe.Nothing
                  , fields = Maybe.maybe game.fields (_ `NonEmptyList.cons` game.fields)
                      $ Field.putPoint (Tuple move.coordinate.x move.coordinate.y) (unwrap move.player)
                      $ NonEmptyList.head game.fields
                  }
              _ ->
                liftEffect $ Console.warn $ "Wrong game to put point"
          Message.DrawResponse gameId color offer ->
            Hooks.modify_ stateId $ case _ of
              AppStateGame game | game.gameId == gameId -> AppStateGame game { drawOffer = if offer then Maybe.Just color else Maybe.Nothing }
              other -> other
          Message.GameResultResponse gameId timeLeft result -> do
            Hooks.modify_ gamesId $ Map.delete gameId
            case state of
              AppStateGame game | game.gameId == gameId -> do
                cancelResignTimer
                Hooks.put resignPendingId false
              _ -> pure unit
            Hooks.modify_ stateId $ case _ of
              AppStateGame game | game.gameId == gameId -> AppStateGame game
                { result = Maybe.Just result
                , timeLeft = { red: Milliseconds $ Int.toNumber timeLeft.red, black: Milliseconds $ Int.toNumber timeLeft.black }
                }
              other -> other
          Message.NicknameChangedResponse playerId player ->
            Hooks.modify_ playersId $ Map.insert playerId player
          Message.NicknameAvailableResponse nickname available ->
            case state of
              AppStateProfile { availability: AvailabilityChecking nickname' } | nickname == nickname' ->
                Hooks.put stateId $ AppStateProfile
                  { availability: (if available then AvailabilityAvailable else AvailabilityTaken) nickname }
              _ -> pure unit
          Message.RatingsUpdatedResponse _ redPlayerId redPlayer blackPlayerId blackPlayer -> do
            Hooks.modify_ playersId $ Map.insert redPlayerId redPlayer
            Hooks.modify_ playersId $ Map.insert blackPlayerId blackPlayer
        pure $ Maybe.Just a

    Hooks.pure
      $ HH.div
          [ HP.class_ $ wrap appRootClass ]
      $
        [ HH.div
            [ HP.class_ $ wrap topBarClass ]
            [ HH.div
                [ HP.class_ $ wrap logoLinkClass
                , HE.onClick $ const do
                    unsubscribe
                    Hooks.put watchingGameIdId Maybe.Nothing
                    Hooks.put stateId AppStateEmpty
                    liftEffect $ putHistoryState AppHistoryStateEmpty
                ]
                [ HH.img
                    [ HP.class_ $ wrap logoImgClass
                    , HP.src "logo.svg"
                    , HP.width 24
                    ]
                , HH.label
                    [ HP.class_ $ wrap navLabelClass ]
                    [ HH.text "Kropki"
                    ]
                ]
            , HH.div
                [ HP.class_ $ wrap gameHeaderCenterClass ] $ case state of
                AppStateGame game ->
                  let
                    gameWinner = game.result >>= case _ of
                      Message.WinGameResult (Message.Color p) _ -> Maybe.Just p
                      _ -> Maybe.Nothing
                  in
                    renderGameHeader
                      game.gameId
                      game.redPlayer.nickname
                      game.blackPlayer.nickname
                      game.timeLeft
                      (Maybe.isJust game.result)
                      gameWinner
                      game.fields
                AppStateLocalGame game ->
                  renderGameHeader
                    "local"
                    "Player 1"
                    "Player 2"
                    game.timeLeft
                    false
                    Maybe.Nothing
                    game.fields
                _ -> []
            , HH.div
                [ HP.class_ $ wrap topBarRightClass ]
                $
                  ( case state of
                      AppStateGame game | Maybe.isNothing game.result ->
                        case Map.lookup game.gameId games of
                          Maybe.Just gameInfo
                            | activePlayerId == Maybe.Just gameInfo.redPlayerId || activePlayerId == Maybe.Just gameInfo.blackPlayerId ->
                                let
                                  myColor =
                                    if activePlayerId == Maybe.Just gameInfo.redPlayerId then Message.Color Player.Red
                                    else Message.Color Player.Black
                                  isMyOffer = game.drawOffer == Maybe.Just myColor
                                  isOpponentOffer = Maybe.isJust game.drawOffer && game.drawOffer /= Maybe.Just myColor
                                  resignClass = btnActionClass <> if resignPending then " " <> btnResignPendingClass else ""
                                  drawClass = btnActionClass <> if isMyOffer then " " <> btnDrawOfferedClass else if isOpponentOffer then " " <> btnDrawOpponentClass else ""
                                  handleDrawClick = do
                                    when isMyOffer $ Hooks.modify_ stateId $ case _ of
                                      AppStateGame g | g.gameId == game.gameId -> AppStateGame g { drawOffer = Maybe.Nothing }
                                      other -> other
                                    Hooks.raise outputToken $ Message.DrawRequest game.gameId
                                in
                                  [ HH.button
                                      [ HP.class_ $ wrap resignClass
                                      , HE.onClick \_ -> handleResignClick game.gameId
                                      ]
                                      [ HH.text "Resign" ]
                                  , HH.button
                                      [ HP.class_ $ wrap drawClass
                                      , HE.onClick \_ -> handleDrawClick
                                      ]
                                      [ HH.text "Draw" ]
                                  ]
                          _ -> []
                      _ -> []
                  )
                    <>
                      [ HH.div
                          [ HP.class_ $ wrap menuWrapperClass
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
                                MenuSaveSgf -> do
                                  state' <- Hooks.get stateId
                                  let
                                    maybeFields = case state' of
                                      AppStateGame game -> Maybe.Just game.fields
                                      AppStateLocalGame game -> Maybe.Just game.fields
                                      _ -> Maybe.Nothing
                                  for_ maybeFields \fields ->
                                    liftEffect $ saveFile "game.sgf" $ Sgf.fieldsToSgf fields
                          ]
                      ]
            ]
        , HH.div
            [ HP.class_ $ wrap mainContentClass ]
            [ HH.slot
                _sidePanel
                unit
                SidePanelComponent.sidePanelComponent
                { activePlayerId, games, openGames, players }
                case _ of
                  SidePanelComponent.WatchGame gameId -> when (Maybe.Just gameId /= watchingGameId) $ switchToGame gameId
                  SidePanelComponent.JoinGame gameId -> Hooks.raise outputToken $ Message.JoinRequest gameId
            , HH.div
                [ HP.class_ $ wrap gameAreaClass ]
                [ case state of
                    AppStateGame game ->
                      let
                        game' = Map.lookup game.gameId games
                        nextPlayer = Field.nextPlayer $ NonEmptyList.head game.fields
                        pointer = Maybe.isNothing game.result && Maybe.isJust activePlayerId &&
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
                            liftEffect playMoveSound
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
                              , redoFields = Nil
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
                              , redoFields: Nil
                              }
                            liftEffect $ putHistoryState AppHistoryStateLocalGame
                          CloseGame gameId -> Hooks.raise outputToken $ Message.CloseRequest gameId
                ]
            ]
        ]

getAuthCookie :: Effect (Maybe String)
getAuthCookie = do
  window <- HTML.window
  storage <- Window.localStorage window
  getItem "kropki_auth" storage

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

sectionHeaderClass :: String
sectionHeaderClass = "section-header"

overlayContainerClass :: String
overlayContainerClass = "overlay-container"

cardPanelClass :: String
cardPanelClass = "card-panel"

tdLabelClass :: String
tdLabelClass = "td-label"

tdValueClass :: String
tdValueClass = "td-value"

inputSmallClass :: String
inputSmallClass = "input-small"

inputCreateClass :: String
inputCreateClass = "input-create"

inputCheckboxClass :: String
inputCheckboxClass = "input-checkbox"

sectionSpacerClass :: String
sectionSpacerClass = "section-spacer"

settingsColumnsClass :: String
settingsColumnsClass = "settings-columns"

settingsColLeftClass :: String
settingsColLeftClass = "settings-col-left"

settingsRowClass :: String
settingsRowClass = "settings-row"

previewColClass :: String
previewColClass = "preview-col"

previewBoxClass :: String
previewBoxClass = "preview-box"

btnGroupClass :: String
btnGroupClass = "btn-group"

btnPrimaryClass :: String
btnPrimaryClass = "btn-primary"

btnPointerClass :: String
btnPointerClass = "btn-pointer"

btnNotAllowedClass :: String
btnNotAllowedClass = "btn-not-allowed"

btnMrClass :: String
btnMrClass = "btn-mr"

inputNicknameClass :: String
inputNicknameClass = "input-nickname"

appRootClass :: String
appRootClass = "app-root"

topBarClass :: String
topBarClass = "top-bar"

logoLinkClass :: String
logoLinkClass = "logo-link"

logoImgClass :: String
logoImgClass = "logo-img"

navLabelClass :: String
navLabelClass = "nav-label"

gameHeaderCenterClass :: String
gameHeaderCenterClass = "game-header-center"

scoreBadgeClass :: String
scoreBadgeClass = "score-badge"

scoreValueClass :: String
scoreValueClass = "score-value"

menuWrapperClass :: String
menuWrapperClass = "menu-wrapper"

mainContentClass :: String
mainContentClass = "main-content"

gameAreaClass :: String
gameAreaClass = "game-area"

menuPosClass :: String
menuPosClass = "menu-pos"

menuTriggerClass :: String
menuTriggerClass = "menu-trigger"

menuDropdownClass :: String
menuDropdownClass = "menu-dropdown"

svgValignMiddleClass :: String
svgValignMiddleClass = "svg-valign-middle"

testNameInputClass :: String
testNameInputClass = "test-name-input"

rememberMeRowClass :: String
rememberMeRowClass = "remember-me-row"

rememberMeCheckboxClass :: String
rememberMeCheckboxClass = "remember-me-checkbox"

rememberMeLabelClass :: String
rememberMeLabelClass = "remember-me-label"

gameActionsClass :: String
gameActionsClass = "game-actions"

topBarRightClass :: String
topBarRightClass = "top-bar-right"

btnActionClass :: String
btnActionClass = "btn-action"

btnResignPendingClass :: String
btnResignPendingClass = "btn-resign-pending"

btnDrawOfferedClass :: String
btnDrawOfferedClass = "btn-draw-offered"

btnDrawOpponentClass :: String
btnDrawOpponentClass = "btn-draw-opponent"

main :: Effect Unit
main = do
  window <- HTML.window
  let connectionEffect = WS.create (if testBuild then "ws://127.0.0.1:8080" else "wss://kropki.org/ws") []
  connection <- connectionEffect
  connectionRef <- Ref.new connection

  location <- Window.location window
  history <- Window.history window
  search <- liftEffect $ Location.search location
  currentPath <- liftEffect $ Location.pathname location
  let
    searchParams = URLSearchParams.fromString search
    maybeGameId = URLSearchParams.get "game" searchParams
    maybeCode = URLSearchParams.get "code" searchParams
    maybeState = URLSearchParams.get "state" searchParams
    maybeAuth = Tuple <$> maybeCode <*> maybeState
    cleanedSearchParams = URLSearchParams.delete "code" $ URLSearchParams.delete "state" searchParams
    cleanedStr = URLSearchParams.toString cleanedSearchParams
    cleanedUrl = currentPath <> if cleanedStr == "" then "" else "?" <> cleanedStr
  History.replaceState (unsafeToForeign AppHistoryStateEmpty) (History.DocumentTitle "kropki") (History.URL cleanedUrl) history

  HA.runHalogenAff do
    body <- HA.awaitBody
    CR.runProcess $ wsProducer connectionRef connectionEffect CR.$$ do
      input <-
        let
          read = CR.await >>= case _ of
            Message.InitResponse playerId players openGames games ->
              pure { playerId, players, openGames, games, watchingGameId: maybeGameId }
            other -> (Console.warn $ "Unexpected first message: " <> show other) *> read
        in
          read

      H.liftEffect $ for_ maybeGameId $ (wsSender connectionRef <<< Message.SubscribeRequest)

      H.liftEffect $ for_ maybeAuth \(Tuple code state) -> do
        maybeAuthCookie <- getAuthCookie
        for_ maybeAuthCookie \authCookie ->
          wsSender connectionRef $ Message.AuthRequest code state authCookie

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
