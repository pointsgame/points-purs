module Test.Main where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, liftMaybe)
import Data.Array as Array
import Data.CodePoint.Unicode as UnicodeCodePoint
import Data.Foldable as Foldable
import Data.FunctorWithIndex as FunctorWithIndex
import Data.List as List
import Data.Maybe as Maybe
import Data.Newtype (unwrap)
import Data.String as String
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits as CodeUnits
import Data.Tuple as Tuple
import Data.Tuple.Nested as NestedTuple
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Exception (Error, error)
import Field (Field, emptyField, isPuttingAllowed, putPoint)
import Player as Player
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

constructField :: forall m. MonadThrow Error m => String -> m Field
constructField image =
  do
    let
      width' = String.length $ String.takeWhile (_ /= codePointFromChar ' ') $ String.dropWhile (_ == codePointFromChar ' ') image
      lines' = Array.filter (not <<< String.null) $ String.split (String.Pattern " ") image
    shouldSatisfy lines' $ List.all (\line -> String.length line == width')
    let
      height' = Array.length lines'
      moves' =
        map (NestedTuple.uncurry3 \x y char -> Tuple.Tuple (Tuple.Tuple x y) if UnicodeCodePoint.isLower char then Player.Red else Player.Black) $
          Array.sortWith (NestedTuple.uncurry3 \_ _ char -> Tuple.Tuple (UnicodeCodePoint.toLower char) (UnicodeCodePoint.isLower char)) do
            Tuple.Tuple y line <- FunctorWithIndex.mapWithIndex (\y line -> Tuple.Tuple y line) lines'
            Tuple.Tuple x char <- FunctorWithIndex.mapWithIndex (\x char -> Tuple.Tuple x char) $ CodeUnits.toCharArray line
            let charCP = codePointFromChar char
            if UnicodeCodePoint.toLower charCP /= UnicodeCodePoint.toUpper charCP then [ NestedTuple.tuple3 x y charCP ] else []
    liftMaybe (error "can't put point") $ Foldable.foldM (\field (Tuple.Tuple pos player) -> putPoint pos player field) (emptyField width' height') moves'

simpleSurround :: Spec Unit
simpleSurround =
  let
    image =
      " .a. \
      \ cBa \
      \ .a. "
  in
    it "simple surround" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 1
      (unwrap field).scoreBlack `shouldEqual` 0
      map Tuple.snd (unwrap field).lastSurroundChain `shouldEqual` Maybe.Just Player.Red
      map (List.length <<< Tuple.fst) (unwrap field).lastSurroundChain `shouldEqual` Maybe.Just 4
      bind (unwrap field).lastSurroundChain (List.last <<< Tuple.fst) `shouldEqual` Maybe.Just (Tuple.Tuple 0 1)

surroundEmptyTerritory :: Spec Unit
surroundEmptyTerritory =
  let
    image =
      " .a. \
      \ a.a \
      \ .a. "
  in
    it "surround empty territory" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 0
      (unwrap field).scoreBlack `shouldEqual` 0
      (unwrap field).lastSurroundChain `shouldEqual` Maybe.Nothing
      shouldSatisfy field $ flip isPuttingAllowed (Tuple.Tuple 1 1)
      shouldSatisfy field $ not <<< flip isPuttingAllowed (Tuple.Tuple 0 1)
      shouldSatisfy field $ not <<< flip isPuttingAllowed (Tuple.Tuple 1 0)
      shouldSatisfy field $ not <<< flip isPuttingAllowed (Tuple.Tuple 1 2)
      shouldSatisfy field $ not <<< flip isPuttingAllowed (Tuple.Tuple 2 1)

movePriority :: Spec Unit
movePriority =
  let
    image =
      " .aB. \
      \ aCaB \
      \ .aB. "
  in
    it "move priority" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 0
      (unwrap field).scoreBlack `shouldEqual` 1
      map Tuple.snd (unwrap field).lastSurroundChain `shouldEqual` Maybe.Just Player.Black
      map (List.length <<< Tuple.fst) (unwrap field).lastSurroundChain `shouldEqual` Maybe.Just 4
      bind (unwrap field).lastSurroundChain (List.last <<< Tuple.fst) `shouldEqual` Maybe.Just (Tuple.Tuple 1 1)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  simpleSurround
  surroundEmptyTerritory
  movePriority
