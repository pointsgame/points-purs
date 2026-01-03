module UsePersistentState where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, parseJson, stringify)
import Data.Either (hush)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple.Nested ((/\), type (/\))
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.Hooks as Hooks
import Web.HTML as HTML
import Web.HTML.Window as Window
import Web.Storage.Storage (getItem, setItem)

type UsePersistentState a = Hooks.UseState a Hooks.<> Hooks.UseEffect Hooks.<> Hooks.Pure

usePersistentState
  :: forall state m
   . MonadEffect m
  => DecodeJson state
  => String
  -> state
  -> Hooks.Hook m (UsePersistentState state) (state /\ Hooks.StateId state)
usePersistentState key state = Hooks.do
  value /\ valueId <- Hooks.useState state

  Hooks.useLifecycleEffect do
    window <- liftEffect $ HTML.window
    storage <- liftEffect $ Window.localStorage window
    str <- liftEffect $ getItem key storage
    maybe (pure unit) (Hooks.put valueId) $ str >>= (parseJson >=> decodeJson) >>> hush
    pure Nothing

  Hooks.pure $ value /\ valueId

put
  :: forall state m
   . MonadEffect m
  => EncodeJson state
  => String
  -> Hooks.StateId state
  -> state
  -> Hooks.HookM m Unit
put key stateId state = do
  window <- liftEffect $ HTML.window
  storage <- liftEffect $ Window.localStorage window
  liftEffect $ setItem key (stringify $ encodeJson state) storage
  Hooks.put stateId state
