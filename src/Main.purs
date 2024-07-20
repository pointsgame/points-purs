module Main where

import Prelude

import CSS.Geometry as CSSGeometry
import CSS.Size as CSSSize
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Exception (error, throwException)
import Field (Field, Pos)
import Field as Field
import Graphics.Canvas (CanvasElement, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Query.Event as HQE
import Halogen.VDom.Driver (runUI)
import Render (DrawSettings, defaultDrawSettings, draw, fromToFieldPos)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (clientHeight, clientWidth)
import Web.Event.Event (EventType(..))
import Web.HTML (HTMLCanvasElement)
import Web.HTML as HTML
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI fieldComponent (NonEmptyList.singleton $ Field.emptyField 10 10) body

foreign import offsetX :: MouseEvent -> Number
foreign import offsetY :: MouseEvent -> Number

toHTMLCanvasElement :: CanvasElement -> HTMLCanvasElement
toHTMLCanvasElement = unsafeCoerce

toPos :: CanvasElement -> Field -> DrawSettings -> Number -> Number -> Effect (Maybe Pos)
toPos canvas field drawSettings x y = do
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas
  let
    fieldWidth = Field.width field
    fieldHeight = Field.height field
    hReflection = drawSettings.hReflection
    vReflection = drawSettings.vReflection
    _ /\ _ /\ toGamePosX /\ toGamePosY /\ _ = fromToFieldPos hReflection vReflection fieldWidth fieldHeight width height
    posX = toGamePosX x
    posY = toGamePosY y
  pure $ if posX >= 0 && posY >= 0 && posX < fieldWidth && posY < fieldHeight then Just (Tuple posX posY) else Nothing

fieldComponent
  :: forall query output m
   . MonadAff m
  => H.Component query (NonEmptyList Field) output m
fieldComponent =
  Hooks.component \_ input -> Hooks.do
    -- TODO: evaluate only if size is changed?
    _ /\ triggerId <- Hooks.useState true

    fields /\ fieldsId <- Hooks.useState input

    let canvasEff = maybe (throwException $ error "no canvas") pure =<< getCanvasElementById "canvas"

    Hooks.useLifecycleEffect do
      window <- liftEffect HTML.window
      subscriptionId <- Hooks.subscribe do
        HQE.eventListener
          (EventType "resize")
          (Window.toEventTarget window)
          (\_ -> Just $ Hooks.modify_ triggerId not)
      pure $ Just $ Hooks.unsubscribe subscriptionId

    Hooks.captures {} Hooks.useTickEffect do
      liftEffect do
        canvas <- canvasEff
        let canvasElement = HTMLCanvasElement.toElement $ toHTMLCanvasElement canvas
        clientWidth canvasElement >>= setCanvasWidth canvas
        clientHeight canvasElement >>= setCanvasHeight canvas
        width <- getCanvasWidth canvas
        height <- getCanvasHeight canvas
        context <- getContext2D canvas
        draw defaultDrawSettings width height fields context
      pure Nothing

    Hooks.pure $ HH.canvas
      [ HP.id "canvas"
      , CSS.style do
          CSSGeometry.width $ CSSSize.pct 100.0
          CSSGeometry.height $ CSSSize.pct 100.0
      , HE.onClick \e -> do
          pos <- liftEffect $ do
            canvas <- canvasEff
            toPos canvas (NonEmptyList.head fields) defaultDrawSettings (offsetX e) (offsetY e)
          maybe (pure unit)
            ( \pos' -> Hooks.modify_ fieldsId $ \fields' -> fromMaybe fields'
                $ map (\x -> NonEmptyList.cons x fields')
                $ Field.putNextPoint pos'
                $ NonEmptyList.head fields'
            )
            pos
      ]
