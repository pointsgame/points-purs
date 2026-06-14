module PolygonMerge (merge, connectHoles) where

import Prelude

import Data.Foldable (foldl)
import Data.List (List(..), concatMap, filter, null, reverse, zip, (:))
import Data.List.NonEmpty as NonEmptyList
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..), fst)
import Field (Chain, Pos, isPosInsideChain, square, w)

type Edge = Tuple Pos Pos

-- | Connect hole polygons to their containing (real) polygons.
-- | Polygons must be ordered clockwise, and holes counter-clockwise.
connectHoles :: List Chain -> List (Tuple Chain (List Chain))
connectHoles polygons =
  let
    Tuple realPolygons holes = split (\p -> square p < 0) polygons
  in
    if null holes then
      map (\p -> Tuple p Nil) realPolygons
    else
      let
        polygonSets = map Set.fromFoldable realPolygons
        boundarySet = Set.unions polygonSets

        findBoundaryPos :: Pos -> Pos
        findBoundaryPos pos = if Set.member pos boundarySet then pos else findBoundaryPos $ w pos

        holesWithBoundaryPos = map (\hole -> Tuple hole $ findBoundaryPos $ NonEmptyList.head hole) holes
      in
        map
          ( \(Tuple polygon polygonSet) ->
              Tuple polygon
                $ map fst
                $ filter
                    ( \(Tuple hole boundaryPos) ->
                        let
                          holePos = NonEmptyList.head hole
                        in
                          Set.member boundaryPos polygonSet && (Set.member holePos polygonSet || isPosInsideChain holePos polygon)
                    )
                    holesWithBoundaryPos
          ) $ zip realPolygons polygonSets

-- | Splits a list into two lists based on a predicate.
-- | The first element of the tuple contains elements that satisfy the predicate.
-- | The second element contains elements that do not.
split :: forall a. (a -> Boolean) -> List a -> Tuple (List a) (List a)
split p xs = loop xs Nil Nil
  where
  loop :: List a -> List a -> List a -> Tuple (List a) (List a)
  loop Nil accTrue accFalse = Tuple (reverse accTrue) (reverse accFalse)
  loop (Cons y ys) accTrue accFalse =
    if p y then loop ys (Cons y accTrue) accFalse
    else loop ys accTrue (Cons y accFalse)

-- | Merges polygons that share a common border.
-- | Polygons must be ordered counter-clockwise.
-- | Preserves ordering based on the input list.
-- | The result polygons are ordered clockwise,
-- | and holes are ordered counter-clockwise.
merge :: List Chain -> List Chain
merge polygons = reconstruct originalVertices adjMap Nil
  where
  -- Flatten all polygons into a single list of edges.
  allEdges :: List Edge
  allEdges = concatMap polyToEdges polygons

  -- Identify boundary edges using XOR logic.
  boundarySet :: Set Edge
  boundarySet = foldl toggleEdge Set.empty allEdges

  -- Build an adjacency map (Pos -> List Pos).
  -- We use a list of targets because a single vertex might connect to multiple
  -- other vertices if two polygons touch at a corner.
  adjMap :: Map Pos (List Pos)
  adjMap = foldl insertEdge Map.empty boundarySet

  insertEdge :: Map Pos (List Pos) -> Edge -> Map Pos (List Pos)
  insertEdge m (Tuple u v) =
    case Map.lookup u m of
      Nothing -> Map.insert u (v : Nil) m
      Just vs -> Map.insert u (vs <> v : Nil) m

  -- We keep a list of all original vertices to determine the order of output polygons.
  originalVertices :: List Pos
  originalVertices = concatMap identity $ map NonEmptyList.toList polygons

-- | Converts a polygon (list of points) into a list of directed edges.
polyToEdges :: Chain -> List Edge
polyToEdges polygon = zip (NonEmptyList.toList polygon) (NonEmptyList.tail polygon <> (NonEmptyList.head polygon : Nil))

-- | The XOR logic for edges.
-- | If the reverse edge exists, remove it (internal border). Otherwise insert.
toggleEdge :: Set Edge -> Edge -> Set Edge
toggleEdge s (Tuple u v) =
  if Set.member (Tuple v u) s then Set.delete (Tuple v u) s
  else Set.insert (Tuple u v) s

-- | Reconstructs polygons from the adjacency map.
reconstruct :: List Pos -> Map Pos (List Pos) -> List Chain -> List Chain
reconstruct Nil _ acc = acc
reconstruct (p : ps) m acc =
  case Map.lookup p m of
    Just (next : _) ->
      -- Found a start of a cycle (p -> next). Trace it.
      let
        result = tracePoly p next m
      in
        reconstruct ps result.newMap (result.poly : acc)
    _ -> reconstruct ps m acc

type TraceResult = { poly :: Chain, newMap :: Map Pos (List Pos) }

-- | Traces a single polygon cycle starting at 'start' and going to 'firstNext'.
tracePoly :: Pos -> Pos -> Map Pos (List Pos) -> TraceResult
tracePoly start firstNext initialMap = go start firstNext initialMap Nil
  where
  go :: Pos -> Pos -> Map Pos (List Pos) -> List Pos -> TraceResult
  go curr nextTarget m acc =
    let
      -- Remove the edge (curr -> nextTarget) from the map.
      newMap = Map.update (removeTarget nextTarget) curr m
      -- Add current point to accumulator.
      newAcc = NonEmptyList.cons' curr acc
    in
      if nextTarget == start then { poly: newAcc, newMap: newMap } -- Cycle closed.
      else
        -- Find where to go from 'nextTarget'.
        case Map.lookup nextTarget newMap of
          Just (n : _) -> go nextTarget n newMap $ NonEmptyList.toList newAcc
          _ ->
            -- Dead end. Return what we have.
            { poly: NonEmptyList.cons nextTarget newAcc, newMap: newMap }

  removeTarget :: Pos -> List Pos -> Maybe (List Pos)
  removeTarget t ts =
    case filter (_ /= t) ts of
      Nil -> Nothing
      remaining -> Just remaining
