module Test.Main where

import Prelude

import Control.Monad.Error.Class (class MonadThrow, liftMaybe)
import Data.Array as Array
import Data.CodePoint.Unicode as UnicodeCodePoint
import Data.Foldable as Foldable
import Data.FunctorWithIndex as FunctorWithIndex
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.Newtype (unwrap)
import Data.String as String
import Data.String.CodePoints (codePointFromChar)
import Data.String.CodeUnits as CodeUnits
import Data.Tuple as Tuple
import Data.Tuple.Nested as NestedTuple
import Effect (Effect)
import Effect.Exception (Error, error)
import Field (Field, emptyField, isPuttingAllowed, putPoint)
import Player as Player
import Test.Spec (Spec, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

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
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` List.singleton 4

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
      (unwrap field).lastSurroundChains `shouldEqual` List.Nil
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
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Black
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` List.singleton 4

movePriorityBig :: Spec Unit
movePriorityBig =
  let
    image =
      " .B.. \
      \ BaB. \
      \ aCaB \
      \ .aB. "
  in
    it "move priority, big" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 0
      (unwrap field).scoreBlack `shouldEqual` 2
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Black
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` (4 List.: 4 List.: List.Nil)

onionSurroundings :: Spec Unit
onionSurroundings =
  let
    image =
      " ...c... \
      \ ..cBc.. \
      \ .cBaBc. \
      \ ..cBc.. \
      \ ...c... "
  in
    it "onion surroundings" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 4
      (unwrap field).scoreBlack `shouldEqual` 0
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` List.singleton 8

deepOnionSurroundings :: Spec Unit
deepOnionSurroundings =
  let
    image =
      " ...D... \
      \ ..DcD.. \
      \ .DcBcD. \
      \ DcBaBcD \
      \ .DcBcD. \
      \ ..DcD.. \
      \ ...D... "
  in
    it "deep onion surroundings" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 0
      (unwrap field).scoreBlack `shouldEqual` 9
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Black
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` List.singleton 12

applyControlSurroundingInSameTurn :: Spec Unit
applyControlSurroundingInSameTurn =
  let
    image =
      " .a. \
      \ aBa \
      \ .a. "
  in
    it "apply 'control' surrounding in same turn" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 1
      (unwrap field).scoreBlack `shouldEqual` 0
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` List.singleton 4

doubleSurround :: Spec Unit
doubleSurround =
  let
    image =
      " .a.a. \
      \ aAbAa \
      \ .a.a. "
  in
    it "double surround" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 2
      (unwrap field).scoreBlack `shouldEqual` 0
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` (4 List.: 4 List.: List.Nil)

doubleSurroundWithEmptyPart :: Spec Unit
doubleSurroundWithEmptyPart =
  let
    image =
      " .b.b.. \
      \ b.zAb. \
      \ .b.b.. "
  in
    it "double surround with empty part" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 1
      (unwrap field).scoreBlack `shouldEqual` 0
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` List.singleton 4
      shouldSatisfy field $ flip isPuttingAllowed (Tuple.Tuple 1 1)
      shouldSatisfy field $ not <<< flip isPuttingAllowed (Tuple.Tuple 3 1)

shouldNotLeaveEmptyInside :: Spec Unit -- TODO: check with another last point
shouldNotLeaveEmptyInside =
  let
    image =
      " .aaaa.. \
      \ a....a. \
      \ a.b...a \
      \ .z.bC.a \
      \ a.b...a \
      \ a....a. \
      \ .aaaa.. "
  in
    it "should not leave empty inside" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 1
      (unwrap field).scoreBlack `shouldEqual` 0
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` List.singleton 18
      shouldSatisfy field $ not <<< flip isPuttingAllowed (Tuple.Tuple 2 3)
      shouldSatisfy field $ not <<< flip isPuttingAllowed (Tuple.Tuple 2 4)
      shouldSatisfy field $ not <<< flip isPuttingAllowed (Tuple.Tuple 2 2)
      shouldSatisfy field $ not <<< flip isPuttingAllowed (Tuple.Tuple 1 3)
      shouldSatisfy field $ not <<< flip isPuttingAllowed (Tuple.Tuple 3 3)
      shouldSatisfy field $ not <<< flip isPuttingAllowed (Tuple.Tuple 1 1)

surroundInOppositeTurn :: Spec Unit
surroundInOppositeTurn =
  let
    image =
      " .a. \
      \ aBa \
      \ .a. "
  in
    it "surround in opposite turn" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 1
      (unwrap field).scoreBlack `shouldEqual` 0
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` List.singleton 4

partlySurroundInOppositeTurn :: Spec Unit
partlySurroundInOppositeTurn =
  let
    image =
      " .a.. \
      \ aBa. \
      \ .a.a \
      \ ..a. "
  in
    it "partly surround in opposite turn" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 1
      (unwrap field).scoreBlack `shouldEqual` 0
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` List.singleton 4
      shouldSatisfy field $ flip isPuttingAllowed (Tuple.Tuple 2 2)

holeInsideSurrounding :: Spec Unit
holeInsideSurrounding =
  let
    image =
      " ....c.... \
      \ ...c.c... \
      \ ..c...c.. \
      \ .c..a..c. \
      \ c..a.a..c \
      \ .c..a..c. \
      \ ..c...c.. \
      \ ...cBc... \
      \ ....d.... "
  in
    it "a hole inside a surrounding" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 1
      (unwrap field).scoreBlack `shouldEqual` 0
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` List.singleton 16
      shouldSatisfy field $ not <<< flip isPuttingAllowed (Tuple.Tuple 4 4)
      shouldSatisfy field $ not <<< flip isPuttingAllowed (Tuple.Tuple 4 1)

holeInsideSurroundingAfterOppositeTurnSurrounding :: Spec Unit
holeInsideSurroundingAfterOppositeTurnSurrounding =
  let
    image =
      " ....b.... \
      \ ...b.b... \
      \ ..b...b.. \
      \ .b..a..b. \
      \ b..a.a..b \
      \ .b..a..b. \
      \ ..b...b.. \
      \ ...bCb... \
      \ ....b.... "
  in
    it "a hole inside a surrounding, after opposite turn surrounding" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 1
      (unwrap field).scoreBlack `shouldEqual` 0
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` List.singleton 16
      shouldSatisfy field $ not <<< flip isPuttingAllowed (Tuple.Tuple 4 4)
      shouldSatisfy field $ not <<< flip isPuttingAllowed (Tuple.Tuple 4 1)

surroundingDoesNotExpand :: Spec Unit
surroundingDoesNotExpand =
  let
    image =
      " ....a.... \
      \ ...a.a... \
      \ ..a.a.a.. \
      \ .a.a.a.a. \
      \ a.a.aBa.a \
      \ .a.a.a.a. \
      \ ..a.a.a.. \
      \ ...a.a... \
      \ ....a.... "
  in
    it "surrounding does not expand" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 1
      (unwrap field).scoreBlack `shouldEqual` 0
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` List.singleton 4
      shouldSatisfy field $ flip isPuttingAllowed (Tuple.Tuple 6 3)
      shouldSatisfy field $ flip isPuttingAllowed (Tuple.Tuple 4 3)
      shouldSatisfy field $ flip isPuttingAllowed (Tuple.Tuple 4 5)
      shouldSatisfy field $ flip isPuttingAllowed (Tuple.Tuple 6 5)
      shouldSatisfy field $ not <<< flip isPuttingAllowed (Tuple.Tuple 5 4)

twoSurroundingsWithCommonBorder :: Spec Unit
twoSurroundingsWithCommonBorder =
  let
    image =
      " .a.. \
      \ aAa. \
      \ .bAa \
      \ ..a. "
  in
    it "2 surroundings with common border" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 2
      (unwrap field).scoreBlack `shouldEqual` 0
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` (4 List.: 4 List.: List.Nil)

twoSurroundingsWithCommonDot :: Spec Unit
twoSurroundingsWithCommonDot =
  let
    image =
      " .a.a. \
      \ aBcBa \
      \ .a.a. "
  in
    it "2 surroundings with common dot" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 2
      (unwrap field).scoreBlack `shouldEqual` 0
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` (4 List.: 4 List.: List.Nil)

threeSurroundingsWithCommonBorders :: Spec Unit
threeSurroundingsWithCommonBorders =
  let
    image =
      " ..a.. \
      \ .aAa. \
      \ ..bAa \
      \ .aAa. \
      \ ..a.. "
  in
    it "3 surroundings with common borders" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 3
      (unwrap field).scoreBlack `shouldEqual` 0
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` (4 List.: 4 List.: 4 List.: List.Nil)

twoSurroundingsWithCommonDotOneBorderlineEmptyPlace :: Spec Unit
twoSurroundingsWithCommonDotOneBorderlineEmptyPlace =
  let
    image =
      " ..a.. \
      \ .aBa. \
      \ ..c.a \
      \ .aBa. \
      \ ..a.. "
  in
    it "2 surroundings with common dot, one borderline empty place" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 2
      (unwrap field).scoreBlack `shouldEqual` 0
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` (4 List.: 4 List.: List.Nil)

ambiguousSurrounding1 :: Spec Unit
ambiguousSurrounding1 =
  let
    image =
      " .aa.aa. \
      \ a..b..a \
      \ a.aAa.a \
      \ a..a..a \
      \ .a...a. \
      \ ..aaa.. "
  in
    it "ambiguous surrounding 1" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 1
      (unwrap field).scoreBlack `shouldEqual` 0
      shouldSatisfy field $ flip isPuttingAllowed (Tuple.Tuple 3 4)
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` List.singleton 4

ambiguousSurrounding2 :: Spec Unit
ambiguousSurrounding2 =
  let
    image =
      " ..aaa.. \
      \ .a...a. \
      \ a..a..a \
      \ a.aAa.a \
      \ a..b..a \
      \ .aa.aa. "
  in
    it "ambiguous surrounding 2" $ do
      field <- constructField image
      (unwrap field).scoreRed `shouldEqual` 1
      (unwrap field).scoreBlack `shouldEqual` 0
      shouldSatisfy field $ flip isPuttingAllowed (Tuple.Tuple 3 1)
      (unwrap field).lastSurroundPlayer `shouldEqual` Player.Red
      map (NonEmptyList.length) (unwrap field).lastSurroundChains `shouldEqual` List.singleton 4

main :: Effect Unit
main = runSpecAndExitProcess [ consoleReporter ] do
  simpleSurround
  surroundEmptyTerritory
  movePriority
  movePriorityBig
  onionSurroundings
  deepOnionSurroundings
  applyControlSurroundingInSameTurn
  doubleSurround
  doubleSurroundWithEmptyPart
  shouldNotLeaveEmptyInside
  surroundInOppositeTurn
  partlySurroundInOppositeTurn
  holeInsideSurrounding
  holeInsideSurroundingAfterOppositeTurnSurrounding
  surroundingDoesNotExpand
  twoSurroundingsWithCommonBorder
  twoSurroundingsWithCommonDot
  threeSurroundingsWithCommonBorders
  twoSurroundingsWithCommonDotOneBorderlineEmptyPlace
  ambiguousSurrounding1
  ambiguousSurrounding2
