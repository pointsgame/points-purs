module FieldComponent where

import Prelude

import CSS.Geometry as CSSGeometry
import CSS.Size as CSSSize
import Control.Monad.Cont (lift)
import Control.Monad.Maybe.Trans (mapMaybeT, runMaybeT)
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
import Graphics.Canvas (CanvasElement, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as CSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Query.Event as HQE
import Render (DrawSettings, defaultDrawSettings, draw, fromToFieldPos)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (clientHeight, clientWidth)
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.HTML (HTMLCanvasElement)
import Web.HTML as HTML
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.Window as Window
import Web.UIEvent.MouseEvent (MouseEvent)

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
      liftEffect $ bind (getCanvasElementById "canvas") $ traverse_ $ \canvas -> do
        let canvasElement = HTMLCanvasElement.toElement $ toHTMLCanvasElement canvas
        clientWidth canvasElement >>= setCanvasWidth canvas
        clientHeight canvasElement >>= setCanvasHeight canvas
        width <- getCanvasWidth canvas
        height <- getCanvasHeight canvas
        context <- getContext2D canvas
        draw defaultDrawSettings width height input context
      pure Nothing

    Hooks.pure $ HH.canvas
      [ HP.id "canvas"
      , CSS.style do
          CSSGeometry.width $ CSSSize.pct 100.0
          CSSGeometry.height $ CSSSize.pct 100.0
      , HE.onClick \e -> void $ runMaybeT $ do
          pos <- mapMaybeT liftEffect $ do
            canvas <- wrap $ getCanvasElementById "canvas"
            wrap $ toPos canvas (NonEmptyList.head input) defaultDrawSettings (offsetX e) (offsetY e)
          when (Field.isPuttingAllowed (NonEmptyList.head input) pos)
            $ lift
            $ Hooks.raise tokens.outputToken
            $ Click pos
      ]
