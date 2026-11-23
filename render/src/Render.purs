module Render where

import Prelude

import Control.Bind (bindFlipped)
import Data.Foldable (foldr, for_, traverse_)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int (floor, toNumber)
import Data.List (List(..), mapMaybe, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Number (abs, pi, sqrt)
import Data.Set as Set
import Data.Tuple (Tuple(..), uncurry)
import Data.Tuple as Tuple
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Field (Field, Pos, Chain)
import Field as Field
import Graphics.Canvas (Context2D, TextAlign(..), TextBaseline(..), arc, beginPath, closePath, fill, fillText, lineTo, moveTo, rect, setFillStyle, setFont, setGlobalAlpha, setLineWidth, setStrokeStyle, setTextAlign, setTextBaseline, stroke)
import Partial.Unsafe as UnsafePartial
import Player (Player)
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
  , coordsTop :: Boolean
  , coordsRight :: Boolean
  , coordsBottom :: Boolean
  , coordsLeft :: Boolean
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
  , coordsTop: true
  , coordsRight: true
  , coordsBottom: true
  , coordsLeft: true
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

-- | Size of the margin reserved for coordinate labels, relative to the cell size.
coordsMarginFactor :: Number
coordsMarginFactor = 0.6

-- | Gap between the grid and coordinate labels, relative to the cell size.
coordsGapFactor :: Number
coordsGapFactor = coordsMarginFactor * 0.15

playerColor :: DrawSettings -> Player -> String
playerColor { redColor, blackColor } player = if player == Player.Red then redColor else blackColor

-- | Radius of a point in pixels.
pointRadiusPx :: DrawSettings -> Number -> Number
pointRadiusPx { pointRadius } cellSize = pointRadius * cellSize / 5.0

-- | Adds a full circle to the current path.
circle :: Context2D -> Number -> Number -> Number -> Effect Unit
circle context x y radius = arc context { x, y, radius, start: 0.0, end: 2.0 * pi, useCounterClockwise: true }

-- | Pixel geometry of a field drawn on a canvas area.
type Layout =
  { -- | Distance between adjacent grid lines.
    cellSize :: Number
  -- | Size of the area covered by the grid cells (without coordinate margins).
  , gridWidth :: Number
  , gridHeight :: Number
  -- | Top left corner of the grid area.
  , shiftX :: Number
  , shiftY :: Number
  -- | Game position to canvas coordinate, snapped to the pixel grid.
  , fromPosX :: Int -> Number
  , fromPosY :: Int -> Number
  -- | Canvas coordinate to game position (may be outside the field).
  , toPosX :: Number -> Int
  , toPosY :: Number -> Int
  }

layout :: DrawSettings -> Field -> Number -> Number -> Layout
layout
  { hReflection
  , vReflection
  , gridThickness
  , coordsTop
  , coordsRight
  , coordsBottom
  , coordsLeft
  }
  field
  width
  height =
  let
    fieldWidth = Field.width field
    fieldHeight = Field.height field
    count b = if b then 1.0 else 0.0
    -- Margins are proportional to the cell size, so pick the cell size such
    -- that the grid together with its margins fits into the area.
    cellSize = min
      (width / (toNumber fieldWidth + coordsMarginFactor * (count coordsLeft + count coordsRight)))
      (height / (toNumber fieldHeight + coordsMarginFactor * (count coordsTop + count coordsBottom)))
    margin b = if b then cellSize * coordsMarginFactor else 0.0
    gridWidth = cellSize * toNumber fieldWidth
    gridHeight = cellSize * toNumber fieldHeight
    shiftX = margin coordsLeft + shift (width - margin coordsLeft - margin coordsRight) gridWidth
    shiftY = margin coordsTop + shift (height - margin coordsTop - margin coordsBottom) gridHeight
    pixelShift = if gridThickness `mod` 2 == 1 then 0.5 else 0.0
    snap coord = toNumber (floor coord) + pixelShift
  in
    { cellSize
    , gridWidth
    , gridHeight
    , shiftX
    , shiftY
    , fromPosX: \x -> snap $ shiftX + fromPosXY hReflection gridWidth fieldWidth x
    , fromPosY: \y -> snap $ shiftY + fromPosXY (not vReflection) gridHeight fieldHeight y
    , toPosX: \coordX -> toPosXY hReflection gridWidth fieldWidth (coordX - shiftX)
    , toPosY: \coordY -> toPosXY (not vReflection) gridHeight fieldHeight (coordY - shiftY)
    }

-- | Surrounded positions, mapped to the index of the field in the history
-- | (oldest first) where they were surrounded for the first time.
surrounded :: NonEmptyList Field -> Map.Map Pos Int
surrounded fields =
  Map.unions
    $ mapWithIndex (\index field -> Set.toMap (insideChains field) $> index)
    $ List.reverse
    $ NonEmptyList.toList fields
  where
  insideChains field = Set.unions $ mapMaybe (insideChain field) $ Field.lastSurroundChains field
  insideChain field chain =
    List.head (NonEmptyList.tail chain) <#> \nextPos ->
      Field.getInsideChain field (UnsafePartial.unsafePartial $ Field.getInnerPos (NonEmptyList.head chain) nextPos) chain

surroundings :: Set.Set Pos -> Boolean -> Boolean -> NonEmptyList Field -> List Chain
surroundings surrounded' fullFill innerSurroundings fields =
  let
    excluded =
      if innerSurroundings then
        Set.empty
      else
        surrounded'
    included pos = not $ Set.member pos excluded
    -- Quadrants around a point, counter-clockwise: two orthogonal neighbours
    -- and the diagonal neighbour between them.
    quadrants =
      (Field.s /\ Field.se /\ Field.e)
        : (Field.e /\ Field.ne /\ Field.n)
        : (Field.n /\ Field.nw /\ Field.w)
        : (Field.w /\ Field.sw /\ Field.s)
        : Nil
    -- Triangles filling the space between the point and its neighbours,
    -- prepended to the accumulator.
    addTriangles field pos player acc = foldr
      ( \(a /\ d /\ b) acc' ->
          let
            owns dir = Field.isOwner field (dir pos) player
            triangle dir1 dir2 = NonEmptyList.cons' pos $ dir1 pos : dir2 pos : Nil
            consIf cond dir1 dir2 rest = if cond then triangle dir1 dir2 : rest else rest
          in
            if owns a && owns b then
              triangle a b : acc'
            else
              consIf (owns a && owns d) a d $ consIf (owns d && owns b) d b acc'
      )
      acc
      quadrants
    fills =
      if fullFill then
        foldr
          ( \(field /\ lastMove) acc -> case lastMove of
              Just (pos /\ player) | included pos -> addTriangles field pos player acc
              _ -> acc
          )
          Nil
          $ List.zip (NonEmptyList.tail fields) (map (List.head <<< Field.moves) $ NonEmptyList.toList fields)
      else
        Nil
  in
    -- Built with `foldr` so that every chain is consed onto the result only once.
    foldr
      ( \field acc ->
          foldr (\chain acc' -> if included (NonEmptyList.head chain) then chain : acc' else acc') acc
            $ Field.lastSurroundChains field
      )
      fills
      $ NonEmptyList.toList fields

mergedSurroundings :: Set.Set Pos -> Boolean -> Boolean -> NonEmptyList Field -> List (Chain /\ List Chain)
mergedSurroundings surrounded' fullFill innerSurroundings fields = connectHoles $ merge $ surroundings surrounded' fullFill innerSurroundings fields

-- | Offsets of the polygon edge between two adjacent positions, so that the
-- | filling covers the points on the edge.
polygonShift :: Number -> Pos -> Pos -> (Number /\ Number) /\ (Number /\ Number)
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
      -- Not adjacent: can happen only for a broken polygon, leave it as is.
      _, _ -> (0.0 /\ 0.0) /\ (0.0 /\ 0.0)

-- | Draws the field. The canvas area is expected to be cleared by the caller.
draw :: DrawSettings -> Map.Map Pos Int -> List (Chain /\ List Chain) -> Number -> Number -> NonEmptyList Field -> Context2D -> Effect Unit
draw
  drawSettings@
  { hReflection
  , vReflection
  , gridThickness
  , gridColor
  , fillingAlpha
  , fullFill
  , extendedFill
  , innerSurroundings
  , coordsTop
  , coordsRight
  , coordsBottom
  , coordsLeft
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
      { cellSize: scale, gridWidth: width', gridHeight: height', shiftX, shiftY, fromPosX, fromPosY } =
        layout drawSettings headField width height
      fromPos (Tuple x y) = Tuple (fromPosX x) (fromPosY y)
      columns = List.range 0 (fieldWidth - 1)
      rows = List.range 0 (fieldHeight - 1)
      verticalLines = map fromPosX columns
      horizontalLines = map fromPosY rows
      coordFontSize = scale * coordsMarginFactor * 0.7
      coordFont = show (floor coordFontSize) <> "px sans-serif"
      labelGap = scale * coordsGapFactor
      pointR = pointRadiusPx drawSettings scale
      color = playerColor drawSettings
      drawLabels align baseline indices toCoords = do
        setTextAlign context align
        setTextBaseline context baseline
        for_ indices \i -> uncurry (fillText context $ show $ i + 1) $ toCoords i
      players = [ Player.Red, Player.Black ]
    setGlobalAlpha context 1.0
    -- Render grid.
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
    -- Render coordinates.
    when (coordsTop || coordsRight || coordsBottom || coordsLeft) do
      setFont context coordFont
      setFillStyle context gridColor
      when coordsTop $ drawLabels AlignCenter BaselineBottom columns \x -> fromPosX x /\ (shiftY - labelGap)
      when coordsBottom $ drawLabels AlignCenter BaselineTop columns \x -> fromPosX x /\ (shiftY + height' + labelGap)
      when coordsLeft $ drawLabels AlignRight BaselineMiddle rows \y -> (shiftX - labelGap) /\ fromPosY y
      when coordsRight $ drawLabels AlignLeft BaselineMiddle rows \y -> (shiftX + width' + labelGap) /\ fromPosY y
    -- Render points.
    for_ (Field.moves headField) \(Tuple (Tuple x y) player) -> do
      beginPath context
      setFillStyle context $ color player
      circle context (fromPosX x) (fromPosY y) pointR
      fill context
    -- Render last point.
    for_ (List.head $ Field.moves headField) \(Tuple (Tuple x y) player) -> do
      beginPath context
      setLineWidth context 2.0
      setStrokeStyle context $ color player
      circle context (fromPosX x) (fromPosY y) (pointR * 5.0 / 3.0)
      stroke context
    -- Render lines, a single path per player: this is faster than filling
    -- every rectangle separately.
    setGlobalAlpha context fillingAlpha
    when (fullFill && extendedFill) do
      let
        rs = pointR / sqrt 2.0
        -- Rectangles are added with normalized coordinates so that all of them
        -- have the same winding and overlaps are filled only once.
        hBar (x1 /\ y) (x2 /\ _) = rect context { x: min x1 x2, y: y - rs, width: abs (x2 - x1), height: 2.0 * rs }
        vBar (x /\ y1) (_ /\ y2) = rect context { x: x - rs, y: min y1 y2, width: 2.0 * rs, height: abs (y2 - y1) }
      for_ players \player -> do
        setFillStyle context $ color player
        beginPath context
        -- Filter by player first: the body below is evaluated strictly even
        -- when it's not run, so it shouldn't be built for other player's moves.
        for_ (List.filter ((_ == player) <<< Tuple.snd) $ Field.moves headField) \(Tuple pos _) -> when (innerSurroundings || not (Map.member pos surrounded')) do
          let
            -- Whether `pos'` is the player's point or was surrounded earlier
            -- than `pos` (or `pos` was never surrounded).
            isOwner pos' = Field.isPlayer headField pos' player
              || Maybe.maybe false (\i -> Maybe.maybe true (i < _) $ Map.lookup pos surrounded') (Map.lookup pos' surrounded')
          when
            ( Field.isPlayer headField (Field.e pos) player
                && not (isOwner $ Field.n pos)
                && not (isOwner $ Field.s pos)
                && not (isOwner $ Field.ne pos)
                && not (isOwner $ Field.se pos)
            )
            $ hBar (fromPos pos) (fromPos $ Field.e pos)
          when
            ( Field.isPlayer headField (Field.s pos) player
                && not (isOwner $ Field.w pos)
                && not (isOwner $ Field.e pos)
                && not (isOwner $ Field.sw pos)
                && not (isOwner $ Field.se pos)
            )
            $ vBar (fromPos pos) (fromPos $ Field.s pos)
        fill context
    -- Rendering surroundings.
    let
      reflectShift (x /\ y) = x * (if hReflection then -1.0 else 1.0) /\ y * (if vReflection then -1.0 else 1.0)
      toPolygon surrounding =
        if extendedFill then
          bindFlipped
            ( \(Tuple pos nextPos) ->
                let
                  shift1 /\ shift2 = polygonShift pointR pos nextPos
                in
                  (fromPos pos + reflectShift shift1) : (fromPos nextPos + reflectShift shift2) : Nil
            )
            $ List.zip (NonEmptyList.toList surrounding)
            $ NonEmptyList.tail surrounding <> List.singleton (NonEmptyList.head surrounding)
        else
          map fromPos $ NonEmptyList.toList surrounding
      tracePolygon chain = case toPolygon chain of
        Nil -> pure unit
        start : rest -> do
          uncurry (moveTo context) start
          traverse_ (uncurry (lineTo context)) rest
          closePath context
    for_ surroundings' \(Tuple surrounding holes) -> do
      setFillStyle context $ color $ if Field.isPlayer headField (NonEmptyList.head surrounding) Player.Red then Player.Red else Player.Black
      beginPath context
      tracePolygon surrounding
      for_ holes tracePolygon
      fill context

-- | Draws the point that would be put at `pos`. The canvas area is expected
-- | to be cleared by the caller.
drawPointer :: DrawSettings -> Number -> Number -> Field -> Pos -> Context2D -> Effect Unit
drawPointer drawSettings width height field pos@(Tuple x y) context =
  when (Field.isPuttingAllowed field pos) do
    let
      { cellSize, fromPosX, fromPosY } = layout drawSettings field width height
      player = Maybe.fromMaybe Player.Red $ map Player.nextPlayer $ Field.lastPlayer field
    setGlobalAlpha context 0.33
    beginPath context
    setFillStyle context $ playerColor drawSettings player
    circle context (fromPosX x) (fromPosY y) (pointRadiusPx drawSettings cellSize)
    fill context
