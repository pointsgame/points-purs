-- | Random opening positions. A port of `oppai-initial`'s `opening` module, see
-- | it for the detailed description of the rules.
module Opening
  ( fits
  , moves
  ) where

import Prelude

import Control.Monad.Rec.Class (untilJust)
import Data.Array as Array
import Data.Foldable (all, foldl, sum)
import Data.Maybe (Maybe(..))
import Data.Ord (abs)
import Data.Traversable (for)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Random (randomBool, randomInt)
import Message (Opening(..))

-- | The smallest field side the quadruple cross is played on.
quadrupleCrossMinSide :: Int
quadrupleCrossMinSide = 18

-- | Whether the opening can be placed on a field of the given size.
fits :: Opening -> Int -> Int -> Boolean
fits QuadrupleCross width height =
  let
    minSide = min width height
    maxSide = max width height
  in
    minSide >= quadrupleCrossMinSide && maxSide <= 2 * minSide
fits _ _ _ = true

-- | Picks random moves of the opening as `(x, y)` coordinates. Moves alternate
-- | between the players starting with the first one, and there is an even
-- | number of them, so the first player moves next.
-- |
-- | Must only be called when `fits` holds.
moves :: Opening -> Int -> Int -> Effect (Array (Tuple Int Int))
moves Cross = cross
moves TripleCross = tripleCross
moves QuadrupleCross = quadrupleCross

-- | Picks a random offset for a pattern of the given size placed on a field of
-- | the given size. The offset deviates from the center by at most a sixth of
-- | the field size, symmetrically.
offset :: Int -> Int -> Effect Int
offset fieldSize size =
  let
    free = max 0 (fieldSize - size)
    deviation = fieldSize / 6
  in
    randomInt (max 0 (free / 2 - deviation)) (min free (free / 2 + free `mod` 2 + deviation))

-- | Shifts the pattern's points by the offset and interleaves the players'
-- | points.
place :: Int -> Int -> Array (Tuple Int Int) -> Array (Tuple Int Int) -> Array (Tuple Int Int)
place xOffset yOffset xPoints oPoints =
  let
    shift (Tuple x y) = Tuple (xOffset + x) (yOffset + y)
  in
    Array.concat $ Array.zipWith (\p q -> [ shift p, shift q ]) xPoints oPoints

-- | A single cross: a 2x2 square with each player's points on one of its
-- | diagonals.
cross :: Int -> Int -> Effect (Array (Tuple Int Int))
cross width height = do
  rotation <- randomBool
  let
    diagonal = [ Tuple 0 0, Tuple 1 1 ]
    antidiagonal = [ Tuple 0 1, Tuple 1 0 ]
  xOffset <- offset width 2
  yOffset <- offset height 2
  pure
    if rotation then
      -- XO
      -- OX
      place xOffset yOffset diagonal antidiagonal
    else
      -- OX
      -- XO
      place xOffset yOffset antidiagonal diagonal

-- | Three crosses sharing points, which makes a 3x4 (or 4x3) pattern of 8
-- | points in one of its four distinct orientations.
tripleCross :: Int -> Int -> Effect (Array (Tuple Int Int))
tripleCross width height = do
  rotation <- randomInt 0 3
  let
    -- .X.
    -- OXO
    -- XOX
    -- .O.
    vertical1 = [ Tuple 1 0, Tuple 1 1, Tuple 0 2, Tuple 2 2 ]
    vertical2 = [ Tuple 1 3, Tuple 1 2, Tuple 0 1, Tuple 2 1 ]
    -- .OX.
    -- XXOO
    -- .OX.
    horizontal1 = [ Tuple 0 1, Tuple 1 1, Tuple 2 0, Tuple 2 2 ]
    horizontal2 = [ Tuple 3 1, Tuple 2 1, Tuple 1 0, Tuple 1 2 ]
    { xPoints, oPoints, w, h } = case rotation of
      0 -> { xPoints: vertical1, oPoints: vertical2, w: 3, h: 4 }
      1 -> { xPoints: vertical2, oPoints: vertical1, w: 3, h: 4 }
      2 -> { xPoints: horizontal1, oPoints: horizontal2, w: 4, h: 3 }
      _ -> { xPoints: horizontal2, oPoints: horizontal1, w: 4, h: 3 }
  xOffset <- offset width w
  yOffset <- offset height h
  pure $ place xOffset yOffset xPoints oPoints

-- | Makes one attempt to place a quadruple cross. Returns `Nothing` if the
-- | attempt violates the rules.
tryQuadrupleCross :: Int -> Int -> Effect (Maybe (Array (Tuple Int Int)))
tryQuadrupleCross width height = do
  let
    xMargin = width / 4
    yMargin = height / 4
  -- Top left points of the crosses.
  crosses <- for (Array.range 1 4) \_ ->
    Tuple <$> randomInt xMargin (width - 2 - xMargin) <*> randomInt yMargin (height - 2 - yMargin)
  let
    sumX = sum $ map fst crosses
    sumY = sum $ map snd crosses
    -- Center rule: `|sum / 4 - (side - 2) / 2| <= 2 / 25 * side`, multiplied by
    -- 100.
    centered = abs (25 * sumX - 50 * (width - 2)) <= 8 * width && abs (25 * sumY - 50 * (height - 2)) <= 8 * height
    area = width * height
    distance2 (Tuple x1 y1) (Tuple x2 y2) =
      let
        distance = max (abs (x1 - x2)) (abs (y1 - y2))
      in
        distance * distance
    distances2 = Array.concat $ Array.mapWithIndex (\i c -> map (distance2 c) $ Array.drop (i + 1) crosses) crosses
    maxDistance2 = foldl max 0 distances2
    -- Distance rules compare `distance^2` with `k^2 * width * height`,
    -- multiplied by the denominator of `k^2`: `0.225^2 = 81 / 1600`,
    -- `0.36^2 = 81 / 625`, `0.54^2 = 729 / 2500`.
    separated = all (\d -> 1600 * d >= 81 * area) distances2
    spread = 625 * maxDistance2 >= 81 * area && 2500 * maxDistance2 <= 729 * area
  if centered && separated && spread then
    map (Just <<< Array.concat) $ for crosses \(Tuple x y) -> do
      direction <- randomBool
      pure
        if direction then
          -- XO
          -- OX
          [ Tuple x y, Tuple x (y + 1), Tuple (x + 1) (y + 1), Tuple (x + 1) y ]
        else
          -- OX
          -- XO
          [ Tuple x (y + 1), Tuple x y, Tuple (x + 1) y, Tuple (x + 1) (y + 1) ]
  else
    pure Nothing

-- | Four crosses spread over the central part of the field. The placement is
-- | sampled by drawing the crosses until all the rules hold.
quadrupleCross :: Int -> Int -> Effect (Array (Tuple Int Int))
quadrupleCross width height = untilJust $ tryQuadrupleCross width height
