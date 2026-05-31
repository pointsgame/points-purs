module SidePanelComponent where

import Prelude

import Data.Array as Array
import Data.Int (round)
import Data.Number.Format (fixed, toStringWith)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Foldable (elem)
import Data.Newtype (wrap)
import Data.Ord (comparing)
import Data.Ord.Down (Down(..))
import Data.Tuple (Tuple(..), snd)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Message as Message
import Type.Proxy (Proxy(..))

data Output
  = WatchGame Message.GameId
  | JoinGame Message.GameId

type Input =
  { activePlayerId :: Maybe Message.PlayerId
  , games :: Message.Games
  , openGames :: Message.OpenGames
  , players :: Message.Players
  }

_games :: Proxy "games"
_games = Proxy

_openGames :: Proxy "openGames"
_openGames = Proxy

_players :: Proxy "players"
_players = Proxy

sidePanelComponent
  :: forall query m
   . MonadAff m
  => H.Component query Input Output m
sidePanelComponent =
  Hooks.component \{ outputToken } input -> Hooks.do
    collapsed /\ collapsedId <- Hooks.useState false
    Hooks.pure $ HH.div
      [ HP.id "side-panel"
      , HP.classes $ map wrap $ [ sidePanelClass ] <> if collapsed then [ sidePanelCollapsedClass ] else []
      ]
      [ HH.button
          [ HP.class_ $ wrap sidePanelToggleClass
          , HP.title $ if collapsed then "Expand" else "Collapse"
          , HE.onClick $ const $ Hooks.modify_ collapsedId not
          ]
          [ HH.text $ if collapsed then "»" else "«" ]
      , HH.div
          [ HP.class_ $ wrap sidePanelContentClass ]
          [ HH.slot
              _games
              unit
              gamesComponent
              (input.activePlayerId /\ input.games)
              \gameId -> Hooks.raise outputToken $ WatchGame gameId
          , HH.slot
              _openGames
              unit
              openGamesComponent
              (input.activePlayerId /\ input.openGames)
              \gameId -> Hooks.raise outputToken $ JoinGame gameId
          , HH.slot_
              _players
              unit
              playersComponent
              input.players
          ]
      ]

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
                          [ HP.class_ $ wrap vsLabelClass ]
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

highVolatilityThreshold :: Number
highVolatilityThreshold = 0.06

playersComponent
  :: forall query output m
   . MonadAff m
  => H.Component query Message.Players output m
playersComponent =
  Hooks.component \_ players -> Hooks.do
    let
      sortedPlayers =
        Array.sortBy (comparing (Down <<< _.rating <<< snd))
          $ Map.toUnfoldableUnordered players
      isHighVolatility player = player.volatility >= highVolatilityThreshold
      lowVolatilityPlayers = Array.filter (not <<< isHighVolatility <<< snd) sortedPlayers
      highVolatilityPlayers = Array.filter (isHighVolatility <<< snd) sortedPlayers
      fmt = toStringWith (fixed 2)
      renderPlayer isItalic (Tuple _ player) =
        HH.div
          [ HP.class_ $ wrap rosterItemRowClass
          , HP.title $ "Rating: " <> fmt player.rating <> "\nDeviation: " <> fmt player.deviation <> "\nVolatility: " <> fmt player.volatility
          ]
          [ HH.div
              [ HP.class_ $ wrap rosterNameClass ]
              [ HH.text player.nickname ]
          , HH.div
              [ HP.class_ $ wrap rosterMetaClass ]
              [ if isItalic then HH.i_ [ HH.text $ show $ round player.rating ]
                else HH.text $ show $ round player.rating
              ]
          ]
    Hooks.pure $ HH.div_
      $ [ HH.div [ HP.class_ $ wrap rosterHeaderClass ] [ HH.text "Players" ] ]
          <> map (renderPlayer false) lowVolatilityPlayers
          <> map (renderPlayer true) highVolatilityPlayers

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

-- CSS class constants
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

vsLabelClass :: String
vsLabelClass = "vs-label"

sidePanelClass :: String
sidePanelClass = "side-panel"

sidePanelCollapsedClass :: String
sidePanelCollapsedClass = "side-panel-collapsed"

sidePanelToggleClass :: String
sidePanelToggleClass = "side-panel-toggle"

sidePanelContentClass :: String
sidePanelContentClass = "side-panel-content"
