module PolygonMerge (merge, Point, Polygon) where

import Prelude

import Data.Foldable (foldl)
import Data.List (List(..), concatMap, filter, reverse, zip, (:))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))

type Point = Tuple Int Int
type Polygon = List Point
type Edge = Tuple Point Point

-- | Merges polygons that share a common border.
-- | Preserves ordering based on the input list.
merge :: List Polygon -> List Polygon
merge polygons = reverse $ reconstruct originalVertices adjMap
  where
  -- Flatten all polygons into a single list of edges.
  allEdges :: List Edge
  allEdges = concatMap polyToEdges polygons

  -- Identify boundary edges using XOR logic.
  boundarySet :: Set Edge
  boundarySet = foldl toggleEdge Set.empty allEdges

  -- Build an adjacency map (Point -> List Point).
  -- We use a list of targets because a single vertex might connect to multiple
  -- other vertices if two polygons touch at a corner.
  adjMap :: Map Point (List Point)
  adjMap = foldl insertEdge Map.empty boundarySet

  insertEdge :: Map Point (List Point) -> Edge -> Map Point (List Point)
  insertEdge m (Tuple u v) =
    case Map.lookup u m of
      Nothing -> Map.insert u (v : Nil) m
      Just vs -> Map.insert u (v : vs) m

  -- We keep a list of all original vertices to determine the order of output polygons.
  originalVertices :: List Point
  originalVertices = concatMap identity polygons

-- | Converts a polygon (list of points) into a list of directed edges.
polyToEdges :: Polygon -> List Edge
polyToEdges Nil = Nil
polyToEdges (x : xs) = zip (x : xs) (xs <> (x : Nil))

-- | The XOR logic for edges.
-- | If the reverse edge exists, remove it (internal border). Otherwise insert.
toggleEdge :: Set Edge -> Edge -> Set Edge
toggleEdge s (Tuple u v) =
  if Set.member (Tuple v u) s then Set.delete (Tuple v u) s
  else Set.insert (Tuple u v) s

-- | Reconstructs polygons from the adjacency map.
reconstruct :: List Point -> Map Point (List Point) -> List Polygon
reconstruct Nil _ = Nil
reconstruct (p : ps) m =
  case Map.lookup p m of
    Just (next : _) ->
      -- Found a start of a cycle (p -> next). Trace it.
      let
        result = tracePoly p next m
      in
        result.poly : reconstruct ps result.newMap
    _ -> reconstruct ps m

type TraceResult = { poly :: Polygon, newMap :: Map Point (List Point) }

-- | Traces a single polygon cycle starting at 'start' and going to 'firstNext'.
tracePoly :: Point -> Point -> Map Point (List Point) -> TraceResult
tracePoly start firstNext initialMap = go start firstNext initialMap Nil
  where
  go :: Point -> Point -> Map Point (List Point) -> List Point -> TraceResult
  go curr nextTarget m acc =
    let
      -- Remove the edge (curr -> nextTarget) from the map.
      newMap = Map.update (removeTarget nextTarget) curr m
      -- Add current point to accumulator.
      newAcc = curr : acc
    in
      if nextTarget == start then { poly: reverse newAcc, newMap: newMap } -- Cycle closed.
      else
        -- Find where to go from 'nextTarget'.
        case Map.lookup nextTarget newMap of
          Just (n : _) -> go nextTarget n newMap newAcc
          _ ->
            -- Dead end. Return what we have.
            { poly: reverse (nextTarget : newAcc), newMap: newMap }

  removeTarget :: Point -> List Point -> Maybe (List Point)
  removeTarget t ts =
    case filter (_ /= t) ts of
      Nil -> Nothing
      remaining -> Just remaining
