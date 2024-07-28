module FieldComponent where

import Prelude

import CSS as CSS
import Control.Monad.Maybe.Trans (mapMaybeT, runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (traverse_)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe(..))
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Field (Field, Pos)
import Field as Field
import Graphics.Canvas (CanvasElement, Context2D, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Query.Event as HQE
import Render (DrawSettings, defaultDrawSettings, draw, drawPointer, fromToFieldPos)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (clientHeight, clientWidth)
import Web.DOM.Node (parentElement)
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.HTML (HTMLCanvasElement)
import Web.HTML as HTML
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)

foreign import offsetX :: MouseEvent -> Number
foreign import offsetY :: MouseEvent -> Number
-- Forces software rendering on canvas, needed because hardware rendering is buggy in firefox.
foreign import getContext2DThatWillReadFrequently :: CanvasElement -> Effect Context2D

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
    gridThickness = drawSettings.gridThickness
    _ /\ _ /\ toGamePosX /\ toGamePosY /\ _ = fromToFieldPos gridThickness hReflection vReflection fieldWidth fieldHeight width height
    posX = toGamePosX x
    posY = toGamePosY y
  pure $ if posX >= 0 && posY >= 0 && posX < fieldWidth && posY < fieldHeight then Just (Tuple posX posY) else Nothing

data Output = Click Pos

fieldComponent
  :: forall query m
   . MonadAff m
  => H.Component query (NonEmptyList Field) Output m
fieldComponent =
  Hooks.component \tokens input -> Hooks.do
    size /\ sizeId <- Hooks.useState Nothing

    Hooks.useLifecycleEffect do
      window <- liftEffect HTML.window
      subscriptionId <- Hooks.subscribe do
        HQE.eventListener
          (EventType "resize")
          (Window.toEventTarget window)
          ( Event.target >=> Window.fromEventTarget >>> map
              ( \w -> do
                  width <- liftEffect $ Window.innerWidth w
                  height <- liftEffect $ Window.innerHeight w
                  Hooks.put sizeId (Just $ Tuple width height)
              )
          )
      pure $ Just $ Hooks.unsubscribe subscriptionId

    Hooks.captures { size, input } Hooks.useTickEffect do
      let
        setCanvasSize canvas = bind (parentElement $ HTMLCanvasElement.toNode $ toHTMLCanvasElement canvas) $ traverse_ \parent -> do
          clientWidth parent >>= setCanvasWidth canvas
          clientHeight parent >>= setCanvasHeight canvas
      liftEffect $ bind (getCanvasElementById "canvas") $ traverse_ $ \canvas -> do
        setCanvasSize canvas
        width <- getCanvasWidth canvas
        height <- getCanvasHeight canvas
        context <- getContext2DThatWillReadFrequently canvas
        draw defaultDrawSettings width height input context
        bind (getCanvasElementById "canvas-pointer") $ traverse_ $ \canvasPointer -> do
          setCanvasSize canvasPointer
      pure Nothing

    Hooks.pure $ HH.div
      [ HCSS.style do
          CSS.position CSS.relative
          CSS.width $ CSS.pct 100.0
          CSS.height $ CSS.pct 100.0
      ]
      [ HH.canvas
          [ HP.id "canvas"
          , HCSS.style do
              CSS.position CSS.absolute
              CSS.top $ CSS.px 0.0
              CSS.left $ CSS.px 0.0
          ]
      , HH.canvas $
          [ HP.id "canvas-pointer"
          , HCSS.style do
              CSS.position CSS.absolute
              CSS.top $ CSS.px 0.0
              CSS.left $ CSS.px 0.0
          , HE.onClick \e -> void $ runMaybeT $ do
              pos <- mapMaybeT liftEffect $ do
                canvas <- wrap $ getCanvasElementById "canvas-pointer"
                wrap $ toPos canvas (NonEmptyList.head input) defaultDrawSettings (offsetX e) (offsetY e)
              when (Field.isPuttingAllowed (NonEmptyList.head input) pos)
                $ lift
                $ Hooks.raise tokens.outputToken
                $ Click pos
          , HE.onMouseMove \e -> liftEffect $ void $ runMaybeT do
              canvas <- wrap $ getCanvasElementById "canvas-pointer"
              width <- lift $ getCanvasWidth canvas
              height <- lift $ getCanvasHeight canvas
              context <- lift $ getContext2D canvas
              pos <- wrap $ toPos canvas (NonEmptyList.head input) defaultDrawSettings (offsetX e) (offsetY e)
              lift $ drawPointer defaultDrawSettings width height input pos context
          ]
      ]
