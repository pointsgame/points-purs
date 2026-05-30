module FieldComponent where

import Prelude

import CSS as CSS
import CSS.Overflow as CSSOverflow
import Control.Monad.Maybe.Trans (mapMaybeT, runMaybeT)
import Control.Monad.Trans.Class (lift)
import Data.Array as Array
import Data.Foldable (traverse_, for_, sum)
import Data.Int (toNumber)
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (wrap)
import Data.Number (sqrt)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (liftEffect)
import Effect.Ref as Ref
import Field (Field, Pos)
import Field as Field
import Graphics.Canvas (CanvasElement, Context2D, clearRect, getCanvasElementById, getCanvasHeight, getCanvasWidth, getContext2D, setCanvasHeight, setCanvasWidth, translate, withContext)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Halogen.Query.Event as HQE
import Render (DrawSettings, draw, drawPointer, fromToFieldPos, coordsMargins, mergedSurroundings, surrounded)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (clientHeight, clientWidth)
import Web.DOM.Node (parentElement)
import Web.Event.Event (EventType(..))
import Web.Event.Event as Event
import Web.HTML (HTMLCanvasElement)
import Web.HTML as HTML
import Web.HTML.HTMLCanvasElement as HTMLCanvasElement
import Web.HTML.Window as Window
import Web.TouchEvent.TouchEvent (TouchEvent)
import Web.TouchEvent.TouchEvent as TouchEvent
import Web.UIEvent.MouseEvent (MouseEvent)
import Web.UIEvent.WheelEvent (WheelEvent)
import Web.UIEvent.WheelEvent as WheelEvent

foreign import offsetX :: MouseEvent -> Number
foreign import offsetY :: MouseEvent -> Number
-- Forces software rendering on canvas, needed because hardware rendering is buggy in firefox.
foreign import getContext2DThatWillReadFrequently :: CanvasElement -> Effect Context2D
foreign import devicePixelRatio :: HTML.Window -> Number
foreign import wheelDeltaY :: WheelEvent -> Number
foreign import wheelOffsetX :: WheelEvent -> Number
foreign import wheelOffsetY :: WheelEvent -> Number
foreign import touchPositions :: TouchEvent -> Effect (Array Point)

-- | A point in canvas CSS pixels relative to the field's top-left corner.
type Point = { x :: Number, y :: Number }

-- | Viewport transform applied before rendering the field.
-- | `scale` is the zoom factor (>= 1, 1 means the whole field fits the viewport)
-- | and `x`/`y` are the pan offsets in CSS pixels (how much of the enlarged
-- | field is scrolled off the top-left).
type Transform = { scale :: Number, x :: Number, y :: Number }

identityTransform :: Transform
identityTransform = { scale: 1.0, x: 0.0, y: 0.0 }

maxScale :: Number
maxScale = 20.0

-- | Movement (in CSS pixels) past which a touch is treated as a pan, not a tap.
moveThreshold :: Number
moveThreshold = 10.0

clampTransform :: Number -> Number -> Transform -> Transform
clampTransform baseW baseH t =
  let
    scale = clamp 1.0 maxScale t.scale
  in
    { scale
    , x: clamp 0.0 (baseW * (scale - 1.0)) t.x
    , y: clamp 0.0 (baseH * (scale - 1.0)) t.y
    }

centroid :: Array Point -> Point
centroid ps =
  let
    n = toNumber (Array.length ps)
  in
    { x: sum (map _.x ps) / n, y: sum (map _.y ps) / n }

-- | Distance between the first two touch points.
pairDistance :: Array Point -> Number
pairDistance ps = fromMaybe 0.0 do
  a <- Array.index ps 0
  b <- Array.index ps 1
  pure $ sqrt ((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y))

-- | Zoom towards the given screen point, keeping that point fixed.
zoomAround :: Number -> Number -> Number -> Number -> Number -> Transform -> Transform
zoomAround baseW baseH cx cy desiredScale t =
  let
    newScale = clamp 1.0 maxScale desiredScale
    ratio = newScale / t.scale
  in
    clampTransform baseW baseH
      { scale: newScale
      , x: (cx + t.x) * ratio - cx
      , y: (cy + t.y) * ratio - cy
      }

-- | Applies a pan/pinch gesture given the previous and current touch points.
applyGesture :: Number -> Number -> Array Point -> Array Point -> Transform -> Transform
applyGesture baseW baseH prev now t =
  let
    pc = centroid prev
    nc = centroid now
    ratio =
      if Array.length prev >= 2 && Array.length now >= 2 then pairDistance now / pairDistance prev
      else 1.0
    newScale = clamp 1.0 maxScale (t.scale * ratio)
    ratioEff = newScale / t.scale
  in
    clampTransform baseW baseH
      { scale: newScale
      , x: (pc.x + t.x) * ratioEff - nc.x
      , y: (pc.y + t.y) * ratioEff - nc.y
      }

toHTMLCanvasElement :: CanvasElement -> HTMLCanvasElement
toHTMLCanvasElement = unsafeCoerce

toPos :: CanvasElement -> Field -> DrawSettings -> Number -> Transform -> Number -> Number -> Effect (Maybe Pos)
toPos canvas field drawSettings dpr transform x y = do
  width <- getCanvasWidth canvas
  height <- getCanvasHeight canvas
  let
    -- Virtual (zoomed) canvas size in device pixels.
    vWidth = width * transform.scale
    vHeight = height * transform.scale
    -- Event coordinates mapped into the virtual canvas space.
    vx = (x + transform.x) * dpr
    vy = (y + transform.y) * dpr
    fieldWidth = Field.width field
    fieldHeight = Field.height field
    hReflection = drawSettings.hReflection
    vReflection = drawSettings.vReflection
    gridThickness = drawSettings.gridThickness
    prelimCellSize = min (vWidth / toNumber fieldWidth) (vHeight / toNumber fieldHeight)
    margins = coordsMargins drawSettings.coordsTop drawSettings.coordsRight drawSettings.coordsBottom drawSettings.coordsLeft prelimCellSize
    _ /\ _ /\ toGamePosX /\ toGamePosY /\ _ = fromToFieldPos gridThickness hReflection vReflection fieldWidth fieldHeight vWidth vHeight margins
    posX = toGamePosX vx
    posY = toGamePosY vy
  pure $ if posX >= 0 && posY >= 0 && posX < fieldWidth && posY < fieldHeight then Just (Tuple posX posY) else Nothing

type Input = { fields :: NonEmptyList Field, pointer :: Boolean, drawSettings :: DrawSettings }

data Output = Click Pos

data Redraw a = Redraw a

fieldComponent
  :: forall m
   . MonadAff m
  => H.Component Redraw Input Output m
fieldComponent =
  Hooks.component \{ queryToken, outputToken } input -> Hooks.do
    surrounded' <- Hooks.captures { field: NonEmptyList.head input.fields } $ flip Hooks.useMemo \_ ->
      surrounded input.fields

    surroundings <-
      Hooks.captures
        { field: NonEmptyList.head input.fields
        , fullFill: input.drawSettings.fullFill
        , innerSurroundings: input.drawSettings.innerSurroundings
        } $ flip Hooks.useMemo \_ ->
        mergedSurroundings (Map.keys surrounded') input.drawSettings.fullFill input.drawSettings.innerSurroundings input.fields

    size /\ sizeId <- Hooks.useState Nothing

    transform /\ transformId <- Hooks.useState identityTransform

    -- Tracks the touch points of the active gesture so that pan/pinch deltas can
    -- be computed between consecutive `touchmove` events.
    _ /\ gestureRef <- Hooks.useRef ([] :: Array Point)

    -- The touch points at the start of the gesture, used to tell a tap apart
    -- from a pan/pinch (so the synthetic click after a pan doesn't place a point).
    _ /\ gestureStartRef <- Hooks.useRef ([] :: Array Point)

    -- Whether the current gesture moved enough to be considered a pan/pinch.
    _ /\ movedRef <- Hooks.useRef false

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

    Hooks.captures { size, transform, field: NonEmptyList.head input.fields, drawSettings: input.drawSettings } Hooks.useTickEffect do
      let
        setCanvasSize canvas = for_ size \{ width, height } -> do
          window <- HTML.window
          let dpr = devicePixelRatio window
          setCanvasWidth canvas $ width * dpr
          setCanvasHeight canvas $ height * dpr
      liftEffect $ bind (getCanvasElementById "canvas") $ traverse_ $ \canvas -> do
        setCanvasSize canvas
        window <- HTML.window
        let
          dpr = devicePixelRatio window
          t = maybe transform (\{ width, height } -> clampTransform width height transform) size
        width <- getCanvasWidth canvas
        height <- getCanvasHeight canvas
        context <- getContext2DThatWillReadFrequently canvas
        clearRect context { x: 0.0, y: 0.0, width, height }
        withContext context do
          translate context { translateX: -t.x * dpr, translateY: -t.y * dpr }
          draw input.drawSettings surrounded' surroundings (width * t.scale) (height * t.scale) input.fields context
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
          , HE.onWheel \e -> do
              liftEffect $ Event.preventDefault $ WheelEvent.toEvent e
              for_ size \{ width, height } -> do
                let
                  cx = wheelOffsetX e
                  cy = wheelOffsetY e
                  factor = if wheelDeltaY e < 0.0 then 1.1 else 1.0 / 1.1
                Hooks.modify_ transformId \t -> zoomAround width height cx cy (t.scale * factor) t
          , HE.onTouchStart \e -> liftEffect do
              positions <- touchPositions e
              Ref.write positions gestureRef
              Ref.write positions gestureStartRef
              Ref.write false movedRef
          , HE.onTouchMove \e -> do
              liftEffect $ Event.preventDefault $ TouchEvent.toEvent e
              prev <- liftEffect $ Ref.read gestureRef
              start <- liftEffect $ Ref.read gestureStartRef
              now <- liftEffect $ touchPositions e
              liftEffect $ Ref.write now gestureRef
              -- Mark the gesture as a move once it pinches or pans past a threshold.
              liftEffect $ when (Array.length now >= 2 || (not (Array.null start) && pairDistance [ centroid start, centroid now ] > moveThreshold)) $
                Ref.write true movedRef
              for_ size \{ width, height } ->
                when (not (Array.null prev) && not (Array.null now))
                  $ Hooks.modify_ transformId
                  $ applyGesture width height prev now
          , HE.onTouchEnd \e -> liftEffect do
              positions <- touchPositions e
              Ref.write positions gestureRef
          ]
      $
        [ HH.canvas
            [ HP.id "canvas"
            , HCSS.style do
                CSS.position CSS.absolute
                CSS.top $ CSS.px 0.0
                CSS.left $ CSS.px 0.0
                CSS.width $ CSS.pct 100.0
                CSS.height $ CSS.pct 100.0
            ]
        , HH.canvas $
            [ HP.id "canvas-pointer"
            , HCSS.style do
                unless input.pointer $ CSS.display CSS.displayNone
                CSS.position CSS.absolute
                CSS.top $ CSS.px 0.0
                CSS.left $ CSS.px 0.0
                CSS.width $ CSS.pct 100.0
                CSS.height $ CSS.pct 100.0
            , HE.onClick \e -> void $ runMaybeT $ do
                -- Ignore the synthetic click that follows a pan/pinch gesture.
                moved <- lift $ liftEffect $ Ref.read movedRef
                lift $ liftEffect $ Ref.write false movedRef
                when (not moved) do
                  pos <- mapMaybeT liftEffect $ do
                    window <- lift HTML.window
                    let dpr = devicePixelRatio window
                    canvas <- wrap $ getCanvasElementById "canvas-pointer"
                    wrap $ toPos canvas (NonEmptyList.head input.fields) input.drawSettings dpr transform (offsetX e) (offsetY e)
                  when (Field.isPuttingAllowed (NonEmptyList.head input.fields) pos)
                    $ lift
                    $ Hooks.raise outputToken
                    $ Click pos
            , HE.onMouseMove \e -> liftEffect do
                void $ runMaybeT do
                  window <- lift HTML.window
                  let dpr = devicePixelRatio window
                  canvas <- wrap $ getCanvasElementById "canvas-pointer"
                  width <- lift $ getCanvasWidth canvas
                  height <- lift $ getCanvasHeight canvas
                  context <- lift $ getContext2D canvas
                  mpos <- lift $ toPos canvas (NonEmptyList.head input.fields) input.drawSettings dpr transform (offsetX e) (offsetY e)
                  lift $ clearRect context { x: 0.0, y: 0.0, width, height }
                  case mpos of
                    Just pos -> lift $ withContext context do
                      translate context { translateX: -transform.x * dpr, translateY: -transform.y * dpr }
                      drawPointer input.drawSettings (width * transform.scale) (height * transform.scale) input.fields pos context
                    Nothing -> pure unit
            , HE.onMouseLeave \_ -> liftEffect $ void $ runMaybeT do
                canvas <- wrap $ getCanvasElementById "canvas-pointer"
                width <- lift $ getCanvasWidth canvas
                height <- lift $ getCanvasHeight canvas
                context <- lift $ getContext2D canvas
                lift $ clearRect context { x: 0.0, y: 0.0, width, height }
            ]
        ]
