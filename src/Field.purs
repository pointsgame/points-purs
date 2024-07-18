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
  , Field
  , width
  , height
  , isFull
  , isPuttingAllowed
  , isPlayer
  , emptyField
  , putPoint
  ) where

import Array2D as Array2D
import Data.Foldable as Foldable
import Data.Int as Int
import Data.List as List
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe as Maybe
import Data.Set as Set
import Data.Tuple as Tuple
import Data.Tuple.Nested as NestedTuple
import Partial.Unsafe as UnsafePartial
import Player (Player(..), nextPlayer)
import Prelude

type Pos = Tuple.Tuple Int Int

count :: forall a. (a -> Boolean) -> List.List a -> Int
count f = List.length <<< List.filter f

uniq :: forall a. Eq a => List.List a -> List.List a
uniq = map NonEmptyList.head <<< List.group

n :: Pos -> Pos
n (Tuple.Tuple x y) = Tuple.Tuple x (y + 1)

s :: Pos -> Pos
s (Tuple.Tuple x y) = Tuple.Tuple x (y - 1)

w :: Pos -> Pos
w (Tuple.Tuple x y) = Tuple.Tuple (x - 1) y

e :: Pos -> Pos
e (Tuple.Tuple x y) = Tuple.Tuple (x + 1) y

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

type Field =
  { scoreRed :: Int
  , scoreBlack :: Int
  , moves :: List.List (Tuple.Tuple Pos Player)
  , lastSurroundChain :: Maybe.Maybe (Tuple.Tuple (List.List Pos) Player)
  , cells :: Array2D.Array2D Cell
  }

width :: Field -> Int
width field = Array2D.width field.cells

height :: Field -> Int
height field = Array2D.height field.cells

isFull :: Field -> Boolean
isFull = Array2D.notElem EmptyCell <<< _.cells

isInside :: Field -> Pos -> Boolean
isInside field (Tuple.Tuple x y) = x >= 0 && y >= 0 && x < width field && y < height field

isPuttingAllowed :: Field -> Pos -> Boolean
isPuttingAllowed field pos =
  case field.cells Array2D.!! pos of
    Maybe.Just EmptyCell -> true
    Maybe.Just (EmptyBaseCell _) -> true
    _ -> false

isPlayer :: Field -> Pos -> Player -> Boolean
isPlayer field pos player =
  case field.cells Array2D.!! pos of
    Maybe.Just (PointCell player') -> player' == player
    Maybe.Just (BaseCell player' _) -> player' == player
    _ -> false

isPlayersPoint :: Field -> Pos -> Player -> Boolean
isPlayersPoint field pos player = field.cells Array2D.!! pos == Maybe.Just (PointCell player)

isCapturedPoint :: Field -> Pos -> Player -> Boolean
isCapturedPoint field pos player = field.cells Array2D.!! pos == Maybe.Just (BaseCell (nextPlayer player) true)

isEmptyBase :: Field -> Pos -> Player -> Boolean
isEmptyBase field pos player = field.cells Array2D.!! pos == Maybe.Just (EmptyBaseCell player)

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
  neighborhood pos = n pos List.: s pos List.: w pos List.: e pos List.: List.Nil

emptyField :: Int -> Int -> Field
emptyField width' height' =
  { scoreRed: 0
  , scoreBlack: 0
  , moves: List.Nil
  , lastSurroundChain: Maybe.Nothing
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
      0, 0 -> se centerPos
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
      0, 0 -> s pos
      1, 0 -> n pos
      (-1), 1 -> s pos
      0, 1 -> w pos
      1, 1 -> w pos

square :: NonEmptyList.NonEmptyList Pos -> Int
square chain = square' chain 0
  where
  square' list acc = case NonEmptyList.uncons list of
    { head: h, tail: List.Nil } -> acc + skewProduct h (NonEmptyList.head chain)
    { head: h1, tail: List.Cons h2 t } -> square' (NonEmptyList.cons' h2 t) (acc + skewProduct h1 h2)
  skewProduct (Tuple.Tuple x1 y1) (Tuple.Tuple x2 y2) = x1 * y2 - y1 * x2

buildChain :: Field -> Pos -> Pos -> Player -> Maybe.Maybe (NonEmptyList.NonEmptyList Pos)
buildChain field startPos nextPos player = if NonEmptyList.length chain > 2 && square chain > 0 then Maybe.Just chain else Maybe.Nothing
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
    | isPlayer field pos player = pos
    | otherwise = getNextPlayerPos centerPos (UnsafePartial.unsafePartial $ getNextPos centerPos pos)

getInputPoints :: Field -> Pos -> Player -> List.List (Tuple.Tuple Pos Pos)
getInputPoints field pos player =
  let
    list1 =
      if not $ isPlayer field (w pos) player then
        if isPlayer field (sw pos) player then List.singleton $ Tuple.Tuple (sw pos) (w pos)
        else if isPlayer field (s pos) player then List.singleton $ Tuple.Tuple (s pos) (w pos)
        else List.Nil
      else
        List.Nil
    list2 =
      if not $ isPlayer field (n pos) player then
        if isPlayer field (nw pos) player then Tuple.Tuple (nw pos) (n pos) List.: list1
        else if isPlayer field (w pos) player then Tuple.Tuple (w pos) (n pos) List.: list1
        else list1
      else
        list1
    list3 =
      if not $ isPlayer field (e pos) player then
        if isPlayer field (ne pos) player then Tuple.Tuple (ne pos) (e pos) List.: list2
        else if isPlayer field (n pos) player then Tuple.Tuple (n pos) (e pos) List.: list2
        else list2
      else
        list2
    list4 =
      if not $ isPlayer field (s pos) player then
        if isPlayer field (se pos) player then Tuple.Tuple (se pos) (s pos) List.: list3
        else if isPlayer field (e pos) player then Tuple.Tuple (e pos) (s pos) List.: list3
        else list3
      else
        list3
  in
    list4

posInsideRing :: Pos -> NonEmptyList.NonEmptyList Pos -> Boolean
posInsideRing (Tuple.Tuple x y) ring =
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
          $ count (\(Tuple.Tuple a (Tuple.Tuple b c)) -> b == y && ((a < b && c > b) || (a > b && c < b)))
          $
            List.zip (NonEmptyList.toList coords')
          $ List.zip (NonEmptyList.tail coords') (List.drop 1 $ NonEmptyList.tail coords')
    Maybe.Nothing -> false

getInsideRing :: Field -> Pos -> NonEmptyList.NonEmptyList Pos -> Set.Set Pos
getInsideRing field startPos ring =
  let
    ringSet = Set.fromFoldable ring
  in
    wave field startPos $ flip Set.member ringSet >>> not

getEmptyBase :: Field -> Pos -> Player -> Tuple.Tuple (NonEmptyList.NonEmptyList Pos) (Set.Set Pos)
getEmptyBase field startPos player = Tuple.Tuple emptyBaseChain $ Set.filter (\pos -> isEmptyBase field pos player) $ getInsideRing field startPos emptyBaseChain
  where
  emptyBaseChain = getEmptyBaseChain (w startPos)
  getEmptyBaseChain pos
    | not $ isPlayer field pos player = getEmptyBaseChain (w pos)
    | otherwise =
        let
          inputPoints = getInputPoints field pos player
          chains = List.mapMaybe (\(Tuple.Tuple chainPos _) -> buildChain field pos chainPos player) inputPoints
          result = List.find (posInsideRing startPos) chains
        in
          Maybe.fromMaybe (getEmptyBaseChain (w pos)) result

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

mergeCaptureChains :: Pos -> List.List (NonEmptyList.NonEmptyList Pos) -> List.List Pos
mergeCaptureChains pos chains =
  Maybe.maybe
    (List.concatMap NonEmptyList.toList chains)
    mergeCaptureChains'
    ( Foldable.find (\c -> NonEmptyList.length c >= 2) $
        NonEmptyList.fromList chains
    )
  where
  mergeCaptureChains' chains' =
    let
      firstChain = NonEmptyList.head chains'
      lastChain = NonEmptyList.last chains'
    in
      if Maybe.Just (NonEmptyList.head firstChain) /= lastChain NonEmptyList.!! (NonEmptyList.length lastChain - 2) then
        List.foldr
          ( \p acc ->
              if p /= pos && List.elem p acc then
                List.dropWhile (_ /= p) acc
              else
                p List.: acc
          )
          List.Nil $ NonEmptyList.toList $ NonEmptyList.concat chains'
      else mergeCaptureChains' $ NonEmptyList.snoc' (NonEmptyList.tail chains') firstChain

putPoint :: Pos -> Player -> Field -> Maybe.Maybe Field
putPoint pos player field
  | not (isPuttingAllowed field pos) = Maybe.Nothing
  | otherwise =
      Maybe.Just $
        let
          enemyPlayer = nextPlayer player
          point = UnsafePartial.unsafePartial $ Array2D.unsafeIndex field.cells pos
          newMoves = (Tuple.Tuple pos player) List.: field.moves
        in
          if point == EmptyBaseCell player then
            field
              { moves = newMoves
              , lastSurroundChain = Maybe.Nothing
              , cells = Array2D.updateAtIndices (List.singleton $ Tuple.Tuple pos $ PointCell player) field.cells
              }
          else
            let
              inputPoints = getInputPoints field pos player
              captures =
                List.mapMaybe
                  ( \(Tuple.Tuple chainPos capturedPos) ->
                      do
                        chain <- buildChain field pos chainPos player
                        let
                          captured = List.fromFoldable $ getInsideRing field capturedPos chain
                          capturedCount' = count (\pos' -> isPlayersPoint field pos' enemyPlayer) captured
                          freedCount' = count (\pos' -> isCapturedPoint field pos' player) captured
                        pure $ NestedTuple.tuple4 chain captured capturedCount' freedCount'
                  )
                  inputPoints
              { yes: realCaptures, no: emptyCaptures } = List.partition ((_ /= 0) <<< NestedTuple.get3) captures
              capturedCount = Foldable.sum $ map NestedTuple.get3 realCaptures
              freedCount = Foldable.sum $ map NestedTuple.get4 realCaptures
              realCaptured = List.concatMap NestedTuple.get2 realCaptures
              captureChain = mergeCaptureChains pos $ map NestedTuple.get1 realCaptures
            in
              if point == EmptyBaseCell enemyPlayer then
                let
                  (Tuple.Tuple enemyEmptyBaseChain enemyEmptyBase) = getEmptyBase field pos enemyPlayer
                in
                  if not $ List.null captures then
                    { scoreRed: if player == Red then field.scoreRed + capturedCount else field.scoreRed - freedCount
                    , scoreBlack: if player == Black then field.scoreBlack + capturedCount else field.scoreBlack - freedCount
                    , moves: newMoves
                    , lastSurroundChain: Maybe.Just (Tuple.Tuple captureChain player)
                    , cells:
                        Array2D.updateAtIndices
                          ( map (\pos' -> Tuple.Tuple pos' EmptyCell) (List.fromFoldable enemyEmptyBase)
                              <> (Tuple.Tuple pos $ PointCell player)
                                List.: map (\pos' -> (Tuple.Tuple pos' $ capture (UnsafePartial.unsafePartial $ Array2D.unsafeIndex field.cells pos') player)) realCaptured
                          )
                          field.cells
                    }
                  else
                    { scoreRed: if player == Red then field.scoreRed else field.scoreRed + 1
                    , scoreBlack: if player == Black then field.scoreBlack else field.scoreBlack + 1
                    , moves: newMoves
                    , lastSurroundChain: Maybe.Just (Tuple.Tuple (NonEmptyList.toList enemyEmptyBaseChain) enemyPlayer)
                    , cells:
                        Array2D.updateAtIndices
                          ( List.snoc
                              (map (\pos' -> Tuple.Tuple pos' $ BaseCell enemyPlayer false) (List.fromFoldable enemyEmptyBase))
                              (Tuple.Tuple pos $ BaseCell enemyPlayer true)
                          )
                          field.cells
                    }
              else
                let
                  newEmptyBase = List.concatMap (List.filter (\pos' -> (UnsafePartial.unsafePartial $ Array2D.unsafeIndex field.cells pos') == EmptyCell) <<< NestedTuple.get2) emptyCaptures
                in
                  { scoreRed: if player == Red then field.scoreRed + capturedCount else field.scoreRed - freedCount
                  , scoreBlack: if player == Black then field.scoreBlack + capturedCount else field.scoreBlack - freedCount
                  , moves: newMoves
                  , lastSurroundChain: if List.null captureChain then Maybe.Nothing else Maybe.Just (Tuple.Tuple captureChain player)
                  , cells:
                      Array2D.updateAtIndices
                        ( (Tuple.Tuple pos $ PointCell player)
                            List.: map (\pos' -> Tuple.Tuple pos' $ EmptyBaseCell player) newEmptyBase
                            <> map (\pos' -> (Tuple.Tuple pos' $ capture (UnsafePartial.unsafePartial $ Array2D.unsafeIndex field.cells pos') player)) realCaptured
                        )
                        field.cells
                  }
