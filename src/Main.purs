module Main where

import Prelude

import Data.List.NonEmpty as NonEmptyList
import Data.Maybe as Maybe
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Field as Field
import FieldComponent (fieldComponent, Output(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.Hooks as Hooks
import Halogen.VDom.Driver (runUI)
import Message as Message
import Type.Proxy (Proxy(..))
import Web.Socket.WebSocket as WS

-- wsSender :: WS.WebSocket -> Message.Request -> Effect Unit
-- wsSender socket request = WS.sendString socket request

_field :: Proxy "field"
_field = Proxy

appComponent
  :: forall query input output m
   . MonadAff m
  => H.Component query input output m
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
    runUI appComponent unit body
