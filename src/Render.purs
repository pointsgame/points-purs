module Render where

import Prelude

import Control.Bind (bindFlipped)
import Control.Monad.Writer (execWriter, tell)
import Data.Foldable (traverse_, for_)
import Data.Function (applyFlipped)
import Data.Functor (voidRight)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (floor, toNumber)
import Data.List (List(..), mapMaybe, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Map as Map
import Data.Maybe (maybe)
import Data.Maybe as Maybe
import Data.Monoid.Endo (Endo(..))
import Data.Newtype (unwrap)
import Data.Number (pi, sqrt)
import Data.Set as Set
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), tuple4, (/\))
import Effect (Effect)
import Field (Field, Pos, Chain)
import Field as Field
import Graphics.Canvas (Context2D, arc, beginPath, clearRect, closePath, fill, lineTo, moveTo, setFillStyle, setGlobalAlpha, setLineWidth, setStrokeStyle, stroke)
import Partial.Unsafe as UnsafePartial
import Player as Player
import PolygonMerge (connectHoles, merge)

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
  , extendedFill :: Boolean
  , innerSurroundings :: Boolean
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
  , extendedFill: true
  , innerSurroundings: true
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

surrounded :: NonEmptyList Field -> Map.Map Pos Int
surrounded fields =
  Map.unions
    $ mapWithIndex
        ( \index field ->
            voidRight index
              $ Set.toMap
              $ Set.unions
              $ mapMaybe
                  ( \chain ->
                      let
                        pos = NonEmptyList.head chain
                        nextPos = List.head $ NonEmptyList.tail chain
                      in
                        map (uncurry $ Field.getInsideChain field)
                          $ map (flip Tuple chain)
                          $ map (UnsafePartial.unsafePartial $ Field.getInnerPos pos) nextPos
                  )
              $
                Field.lastSurroundChains field
        )
    $ List.reverse
    $ NonEmptyList.toList fields

surroundings :: Set.Set Pos -> Boolean -> Boolean -> NonEmptyList Field -> List Chain
surroundings surrounded' fullFill innerSurroundings fields =
  let
    tell' surrounding = tell $ Endo $ (:) surrounding
    surrounded'' =
      if innerSurroundings then
        Set.empty
      else
        surrounded'
  in
    applyFlipped Nil $ unwrap $ execWriter do
      for_ fields \field ->
        for_ (Field.lastSurroundChains field) \chain ->
          when (not $ Set.member (NonEmptyList.head chain) surrounded'') $
            tell' chain
      when fullFill $ for_
        (List.zip (NonEmptyList.tail fields) (map (List.head <<< Field.moves) $ NonEmptyList.toList fields))
        \(Tuple field posPlayer) -> flip (maybe (pure unit)) posPlayer \(Tuple pos player) -> when (not $ Set.member pos surrounded'') do
          if Field.isOwner field (Field.s pos) player && Field.isOwner field (Field.e pos) player then
            tell' $ NonEmptyList.cons' pos $ Field.s pos : Field.e pos : List.Nil
          else do
            when (Field.isOwner field (Field.s pos) player && Field.isOwner field (Field.se pos) player)
              $ tell'
              $ NonEmptyList.cons' pos
              $ Field.s pos : Field.se pos : List.Nil
            when (Field.isOwner field (Field.e pos) player && Field.isOwner field (Field.se pos) player)
              $ tell'
              $ NonEmptyList.cons' pos
              $ Field.se pos : Field.e pos : List.Nil
          if Field.isOwner field (Field.e pos) player && Field.isOwner field (Field.n pos) player then
            tell' $ NonEmptyList.cons' pos $ Field.e pos : Field.n pos : List.Nil
          else do
            when (Field.isOwner field (Field.e pos) player && Field.isOwner field (Field.ne pos) player)
              $ tell'
              $ NonEmptyList.cons' pos
              $ Field.e pos : Field.ne pos : List.Nil
            when (Field.isOwner field (Field.n pos) player && Field.isOwner field (Field.ne pos) player)
              $ tell'
              $ NonEmptyList.cons' pos
              $ Field.ne pos : Field.n pos : List.Nil
          if Field.isOwner field (Field.n pos) player && Field.isOwner field (Field.w pos) player then
            tell' $ NonEmptyList.cons' pos $ Field.n pos : Field.w pos : List.Nil
          else do
            when (Field.isOwner field (Field.n pos) player && Field.isOwner field (Field.nw pos) player)
              $ tell'
              $ NonEmptyList.cons' pos
              $ Field.n pos : Field.nw pos : List.Nil
            when (Field.isOwner field (Field.w pos) player && Field.isOwner field (Field.nw pos) player)
              $ tell'
              $ NonEmptyList.cons' pos
              $ Field.nw pos : Field.w pos : List.Nil
          if Field.isOwner field (Field.w pos) player && Field.isOwner field (Field.s pos) player then
            tell' $ NonEmptyList.cons' pos $ Field.w pos : Field.s pos : List.Nil
          else do
            when (Field.isOwner field (Field.w pos) player && Field.isOwner field (Field.sw pos) player)
              $ tell'
              $ NonEmptyList.cons' pos
              $ Field.w pos : Field.sw pos : List.Nil
            when (Field.isOwner field (Field.s pos) player && Field.isOwner field (Field.sw pos) player)
              $ tell'
              $ NonEmptyList.cons' pos
              $ Field.sw pos : Field.s pos : List.Nil

mergedSurroundings :: Set.Set Pos -> Boolean -> Boolean -> NonEmptyList Field -> List (Chain /\ List Chain)
mergedSurroundings surrounded' fullFill innerSurroundings fields = connectHoles $ merge $ surroundings surrounded' fullFill innerSurroundings fields

polygonShift :: Partial => Number -> Pos -> Pos -> (Number /\ Number) /\ (Number /\ Number)
polygonShift r pos nextPos =
  let
    dx = Tuple.fst pos - Tuple.fst nextPos
    dy = Tuple.snd pos - Tuple.snd nextPos
    rs = r / sqrt 2.0
  in
    case dx, dy of
      -1, -1 -> (0.0 /\ -r) /\ (-r /\ 0.0)
      0, -1 -> (-rs /\ 0.0) /\ (-rs /\ 0.0)
      1, -1 -> (0.0 /\ r) /\ (-r /\ 0.0)
      1, 0 -> (0.0 /\ rs) /\ (0.0 /\ rs)
      1, 1 -> (0.0 /\ r) /\ (r /\ 0.0)
      0, 1 -> (rs /\ 0.0) /\ (rs /\ 0.0)
      (-1), 1 -> (r /\ 0.0) /\ (0.0 /\ -r)
      (-1), 0 -> (0.0 /\ -rs) /\ (0.0 /\ -rs)

polygonShiftWide :: Partial => Number -> Pos -> Pos -> (Number /\ Number) /\ (Number /\ Number)
polygonShiftWide r pos nextPos =
  let
    dx = Tuple.fst pos - Tuple.fst nextPos
    dy = Tuple.snd pos - Tuple.snd nextPos
    rs = r / sqrt 2.0
    s = case dx, dy of
      -1, -1 -> (-rs /\ -rs)
      0, -1 -> (-r /\ 0.0)
      1, -1 -> (-rs /\ rs)
      1, 0 -> (0.0 /\ r)
      1, 1 -> (rs /\ rs)
      0, 1 -> (r /\ 0.0)
      (-1), 1 -> (rs /\ -rs)
      (-1), 0 -> (0.0 /\ -r)
  in
    s /\ s

draw :: DrawSettings -> Map.Map Pos Int -> List (Chain /\ List Chain) -> Number -> Number -> NonEmptyList Field -> Context2D -> Effect Unit
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
  , extendedFill
  , innerSurroundings
  }
  surrounded'
  surroundings'
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
    -- Render background.
    setGlobalAlpha context 1.0
    clearRect context { x: 0.0, y: 0.0, width, height }
    -- Render grig.
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
    -- Render points.
    for_ (Field.moves headField) \(Tuple (Tuple x y) player) -> do
      beginPath context
      setFillStyle context $ if player == Player.Red then redColor else blackColor
      arc context { x: fromPosX x, y: fromPosY y, radius: pointRadius * scale / 5.0, start: 0.0, end: 2.0 * pi, useCounterClockwise: true }
      fill context
    -- Render last point.
    for_ (List.head $ Field.moves headField) \(Tuple (Tuple x y) player) -> do
      beginPath context
      setLineWidth context 2.0
      setStrokeStyle context $ if player == Player.Red then redColor else blackColor
      arc context { x: fromPosX x, y: fromPosY y, radius: pointRadius * scale / 3.0, start: 0.0, end: 2.0 * pi, useCounterClockwise: true }
      stroke context
    -- Render lines.
    setGlobalAlpha context fillingAlpha
    when (fullFill && extendedFill) do
      let rs = pointRadius * scale / 5.0 / sqrt 2.0
      for_ (Field.moves headField) \(Tuple pos player) -> when (innerSurroundings || not (Map.member pos surrounded')) do
        let
          isOwner pos' = Field.isPlayer headField pos' player ||
            Maybe.maybe false (\i -> Maybe.maybe true ((<) i) $ Map.lookup pos surrounded') (Map.lookup pos' surrounded')
        setFillStyle context $ if player == Player.Red then redColor else blackColor
        when
          ( Field.isPlayer headField (Field.e pos) player
              && not (isOwner $ Field.n pos)
              && not (isOwner $ Field.s pos)
              && not (isOwner $ Field.ne pos)
              && not (isOwner $ Field.se pos)
          )
          do
            beginPath context
            uncurry (moveTo context) $ ((+) (0.0 /\ -rs)) $ fromPos pos
            uncurry (lineTo context) $ ((+) (0.0 /\ -rs)) $ fromPos $ Field.e pos
            uncurry (lineTo context) $ ((+) (0.0 /\ rs)) $ fromPos $ Field.e pos
            uncurry (lineTo context) $ ((+) (0.0 /\ rs)) $ fromPos pos
            closePath context
            fill context
        when
          ( Field.isPlayer headField (Field.s pos) player
              && not (isOwner $ Field.w pos)
              && not (isOwner $ Field.e pos)
              && not (isOwner $ Field.sw pos)
              && not (isOwner $ Field.se pos)
          )
          do
            beginPath context
            uncurry (moveTo context) $ ((+) (-rs /\ 0.0)) $ fromPos pos
            uncurry (lineTo context) $ ((+) (-rs /\ 0.0)) $ fromPos $ Field.s pos
            uncurry (lineTo context) $ ((+) (rs /\ 0.0)) $ fromPos $ Field.s pos
            uncurry (lineTo context) $ ((+) (rs /\ 0.0)) $ fromPos pos
            closePath context
            fill context
    -- Rendering surroundings.
    let
      reflectShift (x /\ y) = x * (if hReflection then -1.0 else 1.0) /\ y * (if vReflection then -1.0 else 1.0)
      toPolygon surrounding =
        if extendedFill then
          bindFlipped
            ( \(Tuple pos nextPos) ->
                let
                  shift1 /\ shift2 = UnsafePartial.unsafePartial $ polygonShift (pointRadius * scale / 5.0) pos nextPos
                in
                  (fromPos pos + reflectShift shift1) : (fromPos nextPos + reflectShift shift2) : Nil
            )
            $ List.zip (NonEmptyList.toList surrounding)
            $ NonEmptyList.tail surrounding <> List.singleton (NonEmptyList.head surrounding)
        else
          map fromPos $ NonEmptyList.tail surrounding
    for_ surroundings' \(Tuple surrounding holes) -> do
      setFillStyle context $ if Field.isPlayer headField (NonEmptyList.head surrounding) Player.Red then redColor else blackColor
      beginPath context
      uncurry (moveTo context) $ fromPos $ NonEmptyList.head surrounding
      traverse_ (uncurry (lineTo context)) $ toPolygon surrounding
      closePath context
      for_ holes \hole -> do
        uncurry (moveTo context) $ fromPos $ NonEmptyList.head hole
        traverse_ (uncurry (lineTo context)) $ toPolygon hole
        closePath context
      fill context

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
