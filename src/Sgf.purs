module Sgf
  ( fieldsToSgf
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.String as String
import Data.String.CodeUnits (singleton) as SCU
import Data.Char (fromCharCode)
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..))
import Field as Field
import Player as Player

posToSgf :: Field.Pos -> String
posToSgf (Tuple x y) =
  let
    toChar n = Maybe.fromMaybe 'a' $ fromCharCode (n + 97)
  in
    SCU.singleton (toChar x) <> SCU.singleton (toChar y)

chainToSgf :: Field.Chain -> String
chainToSgf chain = foldMap posToSgf $ NonEmptyList.toList chain

fieldsToSgf :: NonEmptyList.NonEmptyList Field.Field -> String
fieldsToSgf fields =
  let
    fieldsArray = Array.reverse $ Array.fromFoldable fields
    emptyField = NonEmptyList.last fields
    w = Field.width emptyField
    h = Field.height emptyField
    header = "(;GM[40]SZ[" <> show w <> ":" <> show h <> "]RU[russian]"
    moveEntries = Array.mapMaybe identity $ Array.mapWithIndex
      ( \i field ->
          if i == 0 then Maybe.Nothing
          else
            let
              moves = Field.moves field
              chains = Field.lastSurroundChains field
            in
              case List.head moves of
                Maybe.Nothing -> Maybe.Nothing
                Maybe.Just (Tuple pos player) ->
                  let
                    color = case player of
                      Player.Red -> "W"
                      Player.Black -> "B"
                    chainsStr = case chains of
                      List.Nil -> ""
                      _ -> "." <> String.joinWith "" (Array.fromFoldable $ map chainToSgf chains)
                  in
                    Maybe.Just $ ";" <> color <> "[" <> posToSgf pos <> chainsStr <> "]"
      )
      fieldsArray
  in
    header <> String.joinWith "" (Array.fromFoldable moveEntries) <> ")"
