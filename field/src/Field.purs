module Field
  ( Pos
  , n
  , s
  , w
  , e
  , nw
  , ne
  , sw
  , se
  , Cell
  , Field(Field)
  , width
  , height
  , moves
  , lastSurroundPlayer
  , lastSurroundChains
  , scoreRed
  , scoreBlack
  , isFull
  , isPuttingAllowed
  , isOwner
  , isPlayer
  , emptyField
  , putPoint
  , lastPlayer
  , nextPlayer
  , putNextPoint
  , winner
  ) where

import Prelude

import Array2D as Array2D
import Data.Array as Array
import Data.Int as Int
import Data.List (List, (:))
import Data.List as List
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe)
import Data.Maybe as Maybe
import Data.Newtype (class Newtype)
import Data.Set as Set
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Data.Tuple as Tuple
import Data.Unfoldable as Unfoldable
import Partial.Unsafe as UnsafePartial
import Player (Player)
import Player as Player

type Pos = Tuple Int Int

count :: forall a. (a -> Boolean) -> List a -> Int
count f = List.length <<< List.filter f

uniq :: forall a. Eq a => List a -> List a
uniq = map NonEmptyList.head <<< List.group

n :: Pos -> Pos
n (Tuple x y) = Tuple x (y + 1)

s :: Pos -> Pos
s (Tuple x y) = Tuple x (y - 1)

w :: Pos -> Pos
w (Tuple x y) = Tuple (x - 1) y

e :: Pos -> Pos
e (Tuple x y) = Tuple (x + 1) y

nw :: Pos -> Pos
nw = n <<< w

ne :: Pos -> Pos
ne = n <<< e

sw :: Pos -> Pos
sw = s <<< w

se :: Pos -> Pos
se = s <<< e

data Cell
  = EmptyCell
  | PointCell Player
  | BaseCell Player Boolean
  | EmptyBaseCell Player

derive instance Eq Cell

newtype Field =
  Field
    { scoreRed :: Int
    , scoreBlack :: Int
    , moves :: List (Tuple Pos Player)
    , lastSurroundPlayer :: Player
    , lastSurroundChains :: List (NonEmptyList Pos)
    , cells :: Array2D.Array2D Cell
    }

derive instance Newtype Field _

instance Eq Field where
  eq field1 field2 = width field1 == width field2 && height field1 == height field2 && moves field1 == moves field2

instance Show Field where
  show (Field field) =
    fromCharArray $ flip Array.concatMap (Array.range 0 $ height $ Field field) $ \y ->
      flip map (Array.range 0 $ (_ + 1) $ width $ Field field) \x ->
        case field.cells Array2D.!! (Tuple x y) of
          Maybe.Just EmptyCell -> '.'
          Maybe.Just (EmptyBaseCell _) -> '.'
          Maybe.Just (PointCell Player.Red) -> 'X'
          Maybe.Just (PointCell Player.Black) -> 'O'
          Maybe.Just (BaseCell Player.Red true) -> 'o'
          Maybe.Just (BaseCell Player.Black true) -> 'x'
          Maybe.Just (BaseCell _ false) -> ','
          Maybe.Nothing -> '\n'

width :: Field -> Int
width (Field field) = Array2D.width field.cells

height :: Field -> Int
height (Field field) = Array2D.height field.cells

moves :: Field -> List (Tuple Pos Player)
moves (Field field) = field.moves

lastSurroundPlayer :: Field -> Player
lastSurroundPlayer (Field field) = field.lastSurroundPlayer

lastSurroundChains :: Field -> List (NonEmptyList Pos)
lastSurroundChains (Field field) = field.lastSurroundChains

scoreRed :: Field -> Int
scoreRed (Field field) = field.scoreRed

scoreBlack :: Field -> Int
scoreBlack (Field field) = field.scoreBlack

isFull :: Field -> Boolean
isFull (Field field) = Array2D.notElem EmptyCell field.cells

isInside :: Field -> Pos -> Boolean
isInside field (Tuple x y) = x >= 0 && y >= 0 && x < width field && y < height field

isPuttingAllowed :: Field -> Pos -> Boolean
isPuttingAllowed (Field field) pos =
  case field.cells Array2D.!! pos of
    Maybe.Just EmptyCell -> true
    Maybe.Just (EmptyBaseCell _) -> true
    _ -> false

isPlayer :: Field -> Pos -> Player -> Boolean
isPlayer (Field field) pos player =
  case field.cells Array2D.!! pos of
    Maybe.Just (PointCell player') -> player' == player
    Maybe.Just (BaseCell player' true) -> player' /= player
    _ -> false

isOwner :: Field -> Pos -> Player -> Boolean
isOwner (Field field) pos player =
  case field.cells Array2D.!! pos of
    Maybe.Just (PointCell player') -> player' == player
    Maybe.Just (BaseCell player' _) -> player' == player
    _ -> false

isPlayersPoint :: Field -> Pos -> Player -> Boolean
isPlayersPoint (Field field) pos player = field.cells Array2D.!! pos == Maybe.Just (PointCell player)

isCapturedPoint :: Field -> Pos -> Player -> Boolean
isCapturedPoint (Field field) pos player = field.cells Array2D.!! pos == Maybe.Just (BaseCell (Player.nextPlayer player) true)

isEmptyBase :: Field -> Pos -> Player -> Boolean
isEmptyBase (Field field) pos player = field.cells Array2D.!! pos == Maybe.Just (EmptyBaseCell player)

wave :: Field -> Pos -> (Pos -> Boolean) -> Set.Set Pos
wave field startPos f = wave' Set.empty (Set.singleton startPos)
  where
  wave' passed front
    | Set.isEmpty front = passed
    | otherwise = wave' (Set.union passed front) (nextFront passed front)
  nextFront passed front = Set.filter f
    $ Set.fromFoldable
    $
      Set.difference
        ( Set.fromFoldable
            $ List.concatMap (List.filter (isInside field) <<< neighborhood)
            $
              List.fromFoldable front
        )
        passed
  neighborhood pos = n pos : s pos : w pos : e pos : List.Nil

emptyField :: Int -> Int -> Field
emptyField width' height' =
  Field
    { scoreRed: 0
    , scoreBlack: 0
    , moves: List.Nil
    , lastSurroundPlayer: Player.Red
    , lastSurroundChains: List.Nil
    , cells: Array2D.replicate width' height' EmptyCell
    }

getFirstNextPos :: Partial => Pos -> Pos -> Pos
getFirstNextPos centerPos pos =
  let
    dx = Tuple.fst pos - Tuple.fst centerPos
    dy = Tuple.snd pos - Tuple.snd centerPos
  in
    case dx, dy of
      -1, -1 -> se centerPos
      0, -1 -> ne centerPos
      1, -1 -> ne centerPos
      (-1), 0 -> se centerPos
      1, 0 -> nw centerPos
      (-1), 1 -> sw centerPos
      0, 1 -> sw centerPos
      1, 1 -> nw centerPos

getNextPos :: Partial => Pos -> Pos -> Pos
getNextPos centerPos pos =
  let
    dx = Tuple.fst pos - Tuple.fst centerPos
    dy = Tuple.snd pos - Tuple.snd centerPos
  in
    case dx, dy of
      -1, -1 -> e pos
      0, -1 -> e pos
      1, -1 -> n pos
      (-1), 0 -> s pos
      1, 0 -> n pos
      (-1), 1 -> s pos
      0, 1 -> w pos
      1, 1 -> w pos

square :: NonEmptyList Pos -> Int
square chain = square' chain 0
  where
  square' list acc = case NonEmptyList.uncons list of
    { head: h, tail: List.Nil } -> acc + skewProduct h (NonEmptyList.head chain)
    { head: h1, tail: List.Cons h2 t } -> square' (NonEmptyList.cons' h2 t) (acc + skewProduct h1 h2)
  skewProduct (Tuple x1 y1) (Tuple x2 y2) = x1 * y2 - y1 * x2

buildChain :: Field -> Pos -> Pos -> Player -> Maybe (NonEmptyList Pos)
buildChain field startPos nextPos player = if square chain > 0 then Maybe.Just chain else Maybe.Nothing
  where
  chain = getChain startPos $ NonEmptyList.cons' nextPos $ List.singleton startPos
  getChain start list =
    let
      h = NonEmptyList.head list
      nextPos' = getNextPlayerPos h (UnsafePartial.unsafePartial $ getFirstNextPos h start)
    in
      if nextPos' == startPos then list
      else getChain h
        $ Maybe.fromMaybe (NonEmptyList.cons nextPos' list)
        $ NonEmptyList.fromList
        $ NonEmptyList.dropWhile (_ /= nextPos') list
  getNextPlayerPos centerPos pos
    | pos == startPos = pos
    | isOwner field pos player = pos
    | otherwise = getNextPlayerPos centerPos (UnsafePartial.unsafePartial $ getNextPos centerPos pos)

getInputPoints :: Field -> Pos -> Player -> List (Tuple Pos Pos)
getInputPoints field pos player =
  let
    list1 =
      if not $ isOwner field (w pos) player then
        if isOwner field (sw pos) player then List.singleton $ Tuple (sw pos) (w pos)
        else if isOwner field (s pos) player then List.singleton $ Tuple (s pos) (w pos)
        else List.Nil
      else
        List.Nil
    list2 =
      if not $ isOwner field (n pos) player then
        if isOwner field (nw pos) player then Tuple (nw pos) (n pos) : list1
        else if isOwner field (w pos) player then Tuple (w pos) (n pos) : list1
        else list1
      else
        list1
    list3 =
      if not $ isOwner field (e pos) player then
        if isOwner field (ne pos) player then Tuple (ne pos) (e pos) : list2
        else if isOwner field (n pos) player then Tuple (n pos) (e pos) : list2
        else list2
      else
        list2
    list4 =
      if not $ isOwner field (s pos) player then
        if isOwner field (se pos) player then Tuple (se pos) (s pos) : list3
        else if isOwner field (e pos) player then Tuple (e pos) (s pos) : list3
        else list3
      else
        list3
  in
    list4

posInsideRing :: Pos -> NonEmptyList Pos -> Boolean
posInsideRing (Tuple x y) ring =
  case NonEmptyList.fromList $ uniq $ map Tuple.snd $ NonEmptyList.filter ((_ <= x) <<< Tuple.fst) ring of
    Maybe.Just coords ->
      let
        coords'
          | NonEmptyList.last coords == y = NonEmptyList.appendFoldable coords
              $ List.head
              $
                if NonEmptyList.head coords == y then NonEmptyList.tail coords else NonEmptyList.toList coords
          | NonEmptyList.head coords == y = NonEmptyList.cons (NonEmptyList.last coords) coords
          | otherwise = coords
      in
        Int.odd
          $ count (\(Tuple a (Tuple b c)) -> b == y && ((a < b && c > b) || (a > b && c < b)))
          $
            List.zip (NonEmptyList.toList coords')
          $ List.zip (NonEmptyList.tail coords') (List.drop 1 $ NonEmptyList.tail coords')
    Maybe.Nothing -> false

getInsideRing :: Field -> Pos -> NonEmptyList Pos -> Set.Set Pos
getInsideRing field startPos ring =
  let
    ringSet = Set.fromFoldable ring
  in
    wave field startPos $ flip Set.member ringSet >>> not

getEmptyBase :: Field -> Pos -> Player -> Tuple (NonEmptyList Pos) (Set.Set Pos)
getEmptyBase field startPos player = Tuple emptyBaseChain $ Set.filter (\pos -> isEmptyBase field pos player) $ getInsideRing field startPos emptyBaseChain
  where
  emptyBaseChain = getEmptyBaseChain (w startPos)
  getEmptyBaseChain pos
    | not $ isOwner field pos player = getEmptyBaseChain (w pos)
    | otherwise =
        let
          inputPoints = getInputPoints field pos player
          chains = List.mapMaybe (\(Tuple chainPos _) -> buildChain field pos chainPos player) inputPoints
          result = List.find (posInsideRing startPos) chains
        in
          Maybe.fromMaybe' (\_ -> getEmptyBaseChain (w pos)) result

capture :: Cell -> Player -> Cell
capture point player =
  case point of
    EmptyCell -> BaseCell player false
    PointCell player'
      | player' == player -> PointCell player'
      | otherwise -> BaseCell player true
    BaseCell player' enemy
      | player' == player -> BaseCell player' enemy
      | enemy -> PointCell player
      | otherwise -> BaseCell player false
    EmptyBaseCell _ -> BaseCell player false

putPoint :: Pos -> Player -> Field -> Maybe Field
putPoint pos player (Field field)
  | not (isPuttingAllowed (Field field) pos) = Maybe.Nothing
  | otherwise =
      Maybe.Just $
        let
          enemyPlayer = Player.nextPlayer player
          point = UnsafePartial.unsafePartial $ Array2D.unsafeIndex field.cells pos
          newMoves = (Tuple pos player) : field.moves
        in
          if point == EmptyBaseCell player then
            Field field
              { moves = newMoves
              , lastSurroundPlayer = player
              , lastSurroundChains = List.Nil
              , cells = Array2D.updateAtIndices (List.singleton $ Tuple pos $ PointCell player) field.cells
              }
          else
            let
              inputPoints = getInputPoints (Field field) pos player
              fieldWithCaptures =
                List.foldl
                  ( \field' (Tuple chain capturedPos) ->
                      let
                        captured = List.fromFoldable $ getInsideRing (Field field') capturedPos chain
                        capturedCount = count (\pos' -> isPlayersPoint (Field field') pos' enemyPlayer) captured
                        freedCount = count (\pos' -> isCapturedPoint (Field field') pos' player) captured
                      in
                        if capturedCount > 0 then
                          field'
                            { scoreRed = if player == Player.Red then field'.scoreRed + capturedCount else field'.scoreRed - freedCount
                            , scoreBlack = if player == Player.Black then field'.scoreBlack + capturedCount else field'.scoreBlack - freedCount
                            , lastSurroundChains = chain : field'.lastSurroundChains
                            , cells = Array2D.updateAtIndices
                                ( map (\pos' -> (Tuple pos' $ capture (UnsafePartial.unsafePartial $ Array2D.unsafeIndex field.cells pos') player)) captured
                                )
                                field'.cells
                            }
                        else
                          field'
                            { cells = Array2D.updateAtIndices
                                ( map (\pos' -> Tuple pos' $ EmptyBaseCell player) $
                                    List.filter (\pos' -> (UnsafePartial.unsafePartial $ Array2D.unsafeIndex field'.cells pos') == EmptyCell)
                                      captured
                                )
                                field'.cells
                            }
                  )
                  (field { lastSurroundPlayer = player, lastSurroundChains = List.Nil })
                  $ List.sortBy (\(Tuple a _) (Tuple b _) -> compare (NonEmptyList.length a) (NonEmptyList.length b))
                  $
                    inputPoints >>= \(Tuple chainPos capturedPos) ->
                      Unfoldable.fromMaybe
                        $ map (Tuple.swap <<< Tuple capturedPos)
                        $ buildChain (Field field) pos chainPos player
            in
              if point == EmptyBaseCell enemyPlayer then
                if not $ List.null fieldWithCaptures.lastSurroundChains then
                  let
                    enemyEmptyBase = wave (Field fieldWithCaptures) pos (\pos' -> isEmptyBase (Field fieldWithCaptures) pos' enemyPlayer)
                  in
                    Field fieldWithCaptures
                      { moves = newMoves
                      , cells = Array2D.updateAtIndices
                          ( List.snoc (map (\pos' -> Tuple pos' EmptyCell) (List.fromFoldable enemyEmptyBase))
                              $ Tuple pos
                              $ PointCell player
                          )
                          fieldWithCaptures.cells
                      }
                else
                  let
                    (Tuple enemyEmptyBaseChain enemyEmptyBase) = getEmptyBase (Field field) pos enemyPlayer
                  in
                    Field fieldWithCaptures
                      { scoreRed = if player == Player.Red then field.scoreRed else field.scoreRed + 1
                      , scoreBlack = if player == Player.Black then field.scoreBlack else field.scoreBlack + 1
                      , moves = newMoves
                      , lastSurroundPlayer = enemyPlayer
                      , lastSurroundChains = List.singleton enemyEmptyBaseChain
                      , cells = Array2D.updateAtIndices
                          ( List.snoc
                              (map (\pos' -> Tuple pos' $ BaseCell enemyPlayer false) (List.fromFoldable enemyEmptyBase))
                              (Tuple pos $ BaseCell enemyPlayer true)
                          )
                          field.cells
                      }
              else
                Field fieldWithCaptures
                  { moves = newMoves
                  , cells = Maybe.fromMaybe fieldWithCaptures.cells $ Array2D.updateAt pos (PointCell player) fieldWithCaptures.cells
                  }

lastPlayer :: Field -> Maybe Player
lastPlayer (Field field) = map Tuple.snd $ List.head field.moves

nextPlayer :: Field -> Player
nextPlayer = Maybe.fromMaybe Player.Red <<< map Player.nextPlayer <<< lastPlayer

putNextPoint :: Pos -> Field -> Maybe Field
putNextPoint pos field = putPoint pos (nextPlayer field) field

winner :: Field -> Maybe Player
winner (Field field) = case compare field.scoreBlack field.scoreRed of
  LT -> Maybe.Just Player.Red
  GT -> Maybe.Just Player.Black
  EQ -> Maybe.Nothing
