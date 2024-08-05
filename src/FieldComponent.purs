module FieldComponent where

import Prelude

import CSS as CSS
import CSS.Overflow as CSSOverflow
import Control.Monad.Maybe.Trans (mapMaybeT, runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Foldable (traverse_, for_)
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

type Input = { fields :: NonEmptyList Field, pointer :: Boolean }

data Output = Click Pos

data Redraw a = Redraw a

fieldComponent
  :: forall m
   . MonadAff m
  => H.Component Redraw Input Output m
fieldComponent =
  Hooks.component \{ queryToken, outputToken } input -> Hooks.do
    size /\ sizeId <- Hooks.useState Nothing

    let
      setSize = void $ runMaybeT $ (lift <<< Hooks.put sizeId) =<< mapMaybeT liftEffect do
        canvas <- wrap $ getCanvasElementById "canvas"
        parent <- wrap $ parentElement $ HTMLCanvasElement.toNode $ toHTMLCanvasElement canvas
        width <- lift $ clientWidth parent
        height <- lift $ clientHeight parent
        pure $ Just { width, height }

    Hooks.useQuery queryToken \(Redraw a) -> setSize $> Just a

    Hooks.useLifecycleEffect do
      setSize
      window <- liftEffect HTML.window
      subscriptionId <- Hooks.subscribe do
        HQE.eventListener
          (EventType "resize")
          (Window.toEventTarget window)
          (Event.target >=> Window.fromEventTarget >>> map (const setSize))
      pure $ Just $ Hooks.unsubscribe subscriptionId

    Hooks.captures { size, input: input.fields } Hooks.useTickEffect do
      let
        setCanvasSize canvas = for_ size \{ width, height } -> do
          setCanvasWidth canvas width
          setCanvasHeight canvas height
      liftEffect $ bind (getCanvasElementById "canvas") $ traverse_ $ \canvas -> do
        setCanvasSize canvas
        width <- getCanvasWidth canvas
        height <- getCanvasHeight canvas
        context <- getContext2DThatWillReadFrequently canvas
        draw defaultDrawSettings width height input.fields context
        bind (getCanvasElementById "canvas-pointer") $ traverse_ $ \canvasPointer -> do
          setCanvasSize canvasPointer
      pure Nothing

    Hooks.pure
      $ HH.div
          [ HCSS.style do
              CSS.position CSS.relative
              CSS.width $ CSS.pct 100.0
              CSS.height $ CSS.pct 100.0
              CSSOverflow.overflow CSSOverflow.hidden
          ]
      $
        [ HH.canvas
            [ HP.id "canvas"
            , HCSS.style do
                CSS.position CSS.absolute
                CSS.top $ CSS.px 0.0
                CSS.left $ CSS.px 0.0
            ]
        ] <>
          if input.pointer then
            [ HH.canvas $
                [ HP.id "canvas-pointer"
                , HCSS.style do
                    CSS.position CSS.absolute
                    CSS.top $ CSS.px 0.0
                    CSS.left $ CSS.px 0.0
                , HE.onClick \e -> void $ runMaybeT $ do
                    pos <- mapMaybeT liftEffect $ do
                      canvas <- wrap $ getCanvasElementById "canvas-pointer"
                      wrap $ toPos canvas (NonEmptyList.head input.fields) defaultDrawSettings (offsetX e) (offsetY e)
                    when (Field.isPuttingAllowed (NonEmptyList.head input.fields) pos)
                      $ lift
                      $ Hooks.raise outputToken
                      $ Click pos
                , HE.onMouseMove \e -> liftEffect $ void $ runMaybeT do
                    canvas <- wrap $ getCanvasElementById "canvas-pointer"
                    width <- lift $ getCanvasWidth canvas
                    height <- lift $ getCanvasHeight canvas
                    context <- lift $ getContext2D canvas
                    pos <- wrap $ toPos canvas (NonEmptyList.head input.fields) defaultDrawSettings (offsetX e) (offsetY e)
                    lift $ drawPointer defaultDrawSettings width height input.fields pos context
                ]
            ]
          else
            []
