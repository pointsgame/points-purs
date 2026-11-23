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
    initialField = NonEmptyList.last fields
    w = Field.width initialField
    h = Field.height initialField
    header = "(;GM[40]SZ[" <> show w <> ":" <> show h <> "]RU[russian]"
    playerToSgf = case _ of
      Player.Red -> "W"
      Player.Black -> "B"
    -- Moves already on the initial field, e.g. an opening. They don't capture
    -- anything, so there are no chains to write.
    initialEntries = map (\(Tuple pos player) -> ";" <> playerToSgf player <> "[" <> posToSgf pos <> "]")
      $ Array.reverse
      $ Array.fromFoldable
      $ Field.moves initialField
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
                    color = playerToSgf player
                    chainsStr = case chains of
                      List.Nil -> ""
                      _ -> "." <> String.joinWith "" (Array.fromFoldable $ map chainToSgf chains)
                  in
                    Maybe.Just $ ";" <> color <> "[" <> posToSgf pos <> chainsStr <> "]"
      )
      fieldsArray
  in
    header <> String.joinWith "" (initialEntries <> moveEntries) <> ")"
