module Array2D
  ( Array2D
  , width
  , height
  , index
  , (!!)
  , mapWithIndex
  , notElem
  , replicate
  , updateAtIndices
  , unsafeIndex
  ) where

import Data.Array as Array
import Data.Foldable as Foldable
import Data.Functor as Functor
import Data.Maybe as Maybe
import Data.Tuple as Tuple
import Prelude

newtype Array2D a = Array2D
  { width :: Int
  , array :: Array a
  }

derive instance Functor Array2D

width :: forall a. Array2D a -> Int
width (Array2D a) = a.width

height :: forall a. Array2D a -> Int
height (Array2D a) = Array.length a.array `div` a.width

toIndex :: Int -> Int -> Int -> Int
toIndex w x y = y * w + x

toX :: Int -> Int -> Int
toX w idx = idx `mod` w

toY :: Int -> Int -> Int
toY w idx = idx `div` w

index :: forall a. Array2D a -> Tuple.Tuple Int Int -> Maybe.Maybe a
index (Array2D a) (Tuple.Tuple x y) | x >= 0 && y >= 0 && x < a.width && y < height (Array2D a) = a.array Array.!! (toIndex a.width x y)
index _ _ = Maybe.Nothing

infixl 8 index as !!

mapWithIndex :: forall a b. (Tuple.Tuple Int Int -> a -> b) -> Array2D a -> Array2D b
mapWithIndex f (Array2D a) = Array2D a { array = Array.mapWithIndex (\idx -> f $ Tuple.Tuple (toX a.width idx) (toY a.width idx)) a.array }

notElem :: forall a. Eq a => a -> Array2D a -> Boolean
notElem v (Array2D a) = Array.notElem v a.array

replicate :: forall a. Int -> Int -> a -> Array2D a
replicate w h e = Array2D { width: w, array: Array.replicate (w * h) e }

updateAtIndices :: forall t a. Foldable.Foldable t => Functor t => t (Tuple.Tuple (Tuple.Tuple Int Int) a) -> Array2D a -> Array2D a
updateAtIndices elements (Array2D a) = Array2D a
  { array = Array.updateAtIndices (Functor.map (\(Tuple.Tuple (Tuple.Tuple x y) v) -> Tuple.Tuple (toIndex a.width x y) v) elements) a.array
  }

unsafeIndex :: forall a. Partial => Array2D a -> Tuple.Tuple Int Int -> a
unsafeIndex (Array2D a) (Tuple.Tuple x y) | x >= 0 && y >= 0 && x < a.width && y < height (Array2D a) = Array.unsafeIndex a.array $ toIndex a.width x y
