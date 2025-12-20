module Render where

import Prelude

import Data.Foldable (traverse_, for_)
import Data.Int (floor, toNumber)
import Data.List (List, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (maybe)
import Data.Maybe as Maybe
import Data.Number (pi)
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple.Nested (type (/\), tuple4, (/\))
import Effect (Effect)
import Field (Field, Pos)
import Field as Field
import Graphics.Canvas (Context2D, arc, beginPath, clearRect, fill, lineTo, moveTo, setFillStyle, setGlobalAlpha, setLineWidth, setStrokeStyle, stroke)
import Player as Player

type DrawSettings =
  { hReflection :: Boolean
  , vReflection :: Boolean
  , gridThickness :: Int
  , gridColor :: String
  , redColor :: String
  , blackColor :: String
  , pointRadius :: Number
  , fillingAlpha :: Number
  , fullFill :: Boolean
  }

defaultDrawSettings :: DrawSettings
defaultDrawSettings =
  { hReflection: false
  , vReflection: false
  , gridThickness: 2
  , gridColor: "dimgray"
  , redColor: "red"
  , blackColor: "black"
  , pointRadius: 1.0
  , fillingAlpha: 0.5
  , fullFill: true
  }

fromPosXY :: Boolean -> Number -> Int -> Int -> Number
fromPosXY reflection areaSize fieldSize x =
  let
    cellSize = areaSize / toNumber fieldSize
    x' = (toNumber x + 0.5) * cellSize
  in
    if reflection then areaSize - x'
    else x'

toPosXY :: Boolean -> Number -> Int -> Number -> Int
toPosXY reflection areaSize fieldSize x =
  let
    cellSize = areaSize / toNumber fieldSize
    x' = floor $ x / cellSize
  in
    if reflection then fieldSize - x' - 1
    else x'

shift :: Number -> Number -> Number
shift size balancedSize = toNumber $ floor $ (size - balancedSize) / 2.0

dimensions :: Int -> Int -> Number -> Number -> Number /\ Number /\ Number /\ Number /\ Unit
dimensions fieldWidth fieldHeight width height =
  let
    fieldHeight' = toNumber fieldHeight
    fieldWidth' = toNumber fieldWidth
    width' = min width $ height / fieldHeight' * fieldWidth'
    height' = min height $ width / fieldWidth' * fieldHeight'
    shiftX = shift width width'
    shiftY = shift height height'
  in
    width' /\ height' /\ shiftX /\ shiftY /\ unit

fromToFieldPos :: Int -> Boolean -> Boolean -> Int -> Int -> Number -> Number -> (Int -> Number) /\ (Int -> Number) /\ (Number -> Int) /\ (Number -> Int) /\ Unit
fromToFieldPos gridThickness hReflection vReflection fieldWidth fieldHeight width height =
  let
    width' /\ height' /\ shiftX /\ shiftY /\ _ = dimensions fieldWidth fieldHeight width height
    pixelShift = if gridThickness `mod` 2 == 1 then 0.5 else 0.0
  in
    tuple4
      ((_ + pixelShift) <<< toNumber <<< floor <<< (shiftX + _) <<< fromPosXY hReflection width' fieldWidth) -- fromGamePosX
      ((_ + pixelShift) <<< toNumber <<< floor <<< (shiftY + _) <<< fromPosXY (not vReflection) height' fieldHeight) -- fromGamePosY
      (\coordX -> toPosXY hReflection width' fieldWidth (coordX - shiftX)) -- toGamePosX
      (\coordY -> toPosXY (not vReflection) height' fieldHeight (coordY - shiftY)) -- toGamePosY

polygon :: Context2D -> List (Tuple Number Number) -> Effect Unit
polygon _ List.Nil = pure unit
polygon context (h : t) = do
  beginPath context
  uncurry (moveTo context) h
  traverse_ (uncurry (lineTo context)) t
  fill context

draw :: DrawSettings -> Number -> Number -> NonEmptyList Field -> Context2D -> Effect Unit
draw
  { hReflection
  , vReflection
  , gridThickness
  , gridColor
  , redColor
  , blackColor
  , pointRadius
  , fillingAlpha
  , fullFill
  }
  width
  height
  fields
  context =
  do
    let
      headField = NonEmptyList.head fields
      fieldWidth = Field.width headField
      fieldHeight = Field.height headField
      width' /\ height' /\ shiftX /\ shiftY /\ _ = dimensions fieldWidth fieldHeight width height
      scale = width' / toNumber fieldWidth
      fromPosX /\ fromPosY /\ _ /\ _ /\ _ = fromToFieldPos gridThickness hReflection vReflection fieldWidth fieldHeight width height
      fromPos (Tuple x y) = Tuple (fromPosX x) (fromPosY y)
      verticalLines = map fromPosX $ List.range 0 (fieldWidth - 1)
      horizontalLines = map fromPosY $ List.range 0 (fieldHeight - 1)
    --Rendering background.
    setGlobalAlpha context 1.0
    clearRect context { x: 0.0, y: 0.0, width, height }
    --Rendering grig.
    setLineWidth context $ toNumber gridThickness
    setStrokeStyle context gridColor
    for_ verticalLines \x -> do
      beginPath context
      moveTo context x shiftY
      lineTo context x (shiftY + height')
      stroke context
    for_ horizontalLines \y -> do
      beginPath context
      moveTo context shiftX y
      lineTo context (shiftX + width') y
      stroke context
    --Rendering points.
    for_ (Field.moves headField) \(Tuple (Tuple x y) player) -> do
      beginPath context
      setFillStyle context $ if player == Player.Red then redColor else blackColor
      arc context { x: fromPosX x, y: fromPosY y, radius: pointRadius * scale / 5.0, start: 0.0, end: 2.0 * pi, useCounterClockwise: true }
      fill context
    --Rendering last point.
    for_ (List.head $ Field.moves headField) \(Tuple (Tuple x y) player) -> do
      beginPath context
      setLineWidth context 2.0
      setStrokeStyle context $ if player == Player.Red then redColor else blackColor
      arc context { x: fromPosX x, y: fromPosY y, radius: pointRadius * scale / 3.0, start: 0.0, end: 2.0 * pi, useCounterClockwise: true }
      stroke context
    --Rendering little surroundings.
    setGlobalAlpha context fillingAlpha
    when fullFill
      $ for_
          ( List.zip
              (NonEmptyList.toList $ NonEmptyList.reverse fields)
              (map (List.head <<< Field.moves) $ NonEmptyList.tail $ NonEmptyList.reverse fields)
          )
          \(Tuple field posPlayer) -> flip (maybe (pure unit)) posPlayer \(Tuple pos player) -> do
            setFillStyle context $ if player == Player.Red then redColor else blackColor
            if Field.isPlayer field (Field.s pos) player && Field.isPlayer field (Field.e pos) player then
              polygon context $ fromPos pos : fromPos (Field.s pos) : fromPos (Field.e pos) : List.Nil
            else do
              when (Field.isPlayer field (Field.s pos) player && Field.isPlayer field (Field.se pos) player)
                $ polygon context
                $ fromPos pos : fromPos (Field.s pos) : fromPos (Field.se pos) : List.Nil
              when (Field.isPlayer field (Field.e pos) player && Field.isPlayer field (Field.se pos) player)
                $ polygon context
                $ fromPos pos : fromPos (Field.e pos) : fromPos (Field.se pos) : List.Nil
            if Field.isPlayer field (Field.e pos) player && Field.isPlayer field (Field.n pos) player then
              polygon context $ fromPos pos : fromPos (Field.e pos) : fromPos (Field.n pos) : List.Nil
            else do
              when (Field.isPlayer field (Field.e pos) player && Field.isPlayer field (Field.ne pos) player)
                $ polygon context
                $ fromPos pos : fromPos (Field.e pos) : fromPos (Field.ne pos) : List.Nil
              when (Field.isPlayer field (Field.n pos) player && Field.isPlayer field (Field.ne pos) player)
                $ polygon context
                $ fromPos pos : fromPos (Field.n pos) : fromPos (Field.ne pos) : List.Nil
            if Field.isPlayer field (Field.n pos) player && Field.isPlayer field (Field.w pos) player then
              polygon context $ fromPos pos : fromPos (Field.n pos) : fromPos (Field.w pos) : List.Nil
            else do
              when (Field.isPlayer field (Field.n pos) player && Field.isPlayer field (Field.nw pos) player)
                $ polygon context
                $ fromPos pos : fromPos (Field.n pos) : fromPos (Field.nw pos) : List.Nil
              when (Field.isPlayer field (Field.w pos) player && Field.isPlayer field (Field.nw pos) player)
                $ polygon context
                $ fromPos pos : fromPos (Field.w pos) : fromPos (Field.nw pos) : List.Nil
            if Field.isPlayer field (Field.w pos) player && Field.isPlayer field (Field.s pos) player then
              polygon context $ fromPos pos : fromPos (Field.w pos) : fromPos (Field.s pos) : List.Nil
            else do
              when (Field.isPlayer field (Field.w pos) player && Field.isPlayer field (Field.sw pos) player)
                $ polygon context
                $ fromPos pos : fromPos (Field.w pos) : fromPos (Field.sw pos) : List.Nil
              when (Field.isPlayer field (Field.s pos) player && Field.isPlayer field (Field.sw pos) player)
                $ polygon context
                $ fromPos pos : fromPos (Field.s pos) : fromPos (Field.sw pos) : List.Nil
    --Rendering surroundings.
    for_ (NonEmptyList.reverse fields) \field ->
      when (not $ List.null $ Field.lastSurroundChains field) $ for_ (Field.lastSurroundChains field) \chain -> do
        setFillStyle context $ if Field.lastSurroundPlayer field == Player.Red then redColor else blackColor
        polygon context $ map fromPos $ NonEmptyList.toList chain

drawPointer :: DrawSettings -> Number -> Number -> NonEmptyList Field -> Pos -> Context2D -> Effect Unit
drawPointer
  { hReflection
  , vReflection
  , gridThickness
  , redColor
  , blackColor
  , pointRadius
  }
  width
  height
  fields
  pos@(Tuple x y)
  context =
  let
    headField = NonEmptyList.head fields
  in
    do
      clearRect context { x: 0.0, y: 0.0, width, height }
      when (Field.isPuttingAllowed headField pos) $
        do
          let
            fieldWidth = Field.width headField
            fieldHeight = Field.height headField
            width' /\ _ /\ _ /\ _ /\ _ = dimensions fieldWidth fieldHeight width height
            scale = width' / toNumber fieldWidth
            fromPosX /\ fromPosY /\ _ /\ _ /\ _ = fromToFieldPos gridThickness hReflection vReflection fieldWidth fieldHeight width height
            player = Maybe.fromMaybe Player.Red $ map Player.nextPlayer $ Field.lastPlayer headField
          setGlobalAlpha context 0.33
          beginPath context
          setFillStyle context $ if player == Player.Red then redColor else blackColor
          arc context { x: fromPosX x, y: fromPosY y, radius: pointRadius * scale / 5.0, start: 0.0, end: 2.0 * pi, useCounterClockwise: true }
          fill context
