module Main where

import Prelude

import Field (Field)
import Field as Field
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Data.Array ((..))
import Data.Array as Array
import Data.Int as Int
import Data.Maybe as Maybe
import Data.String as String
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff as Aff
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Node.ReadLine as ReadLine
import Node.ReadLine.Aff as ReadLineAff

allMoves :: Int -> Int -> Array Field.Pos
allMoves width height = do
  x <- 0 .. (width - 1)
  y <- 0 .. (height - 1)
  pure $ Tuple x y

main :: Effect Unit
main = do
  interface <- ReadLine.createConsoleInterface ReadLine.noCompletion
  let
    width = 20
    height = 20
    initField = Field.emptyField width height

    loop :: Field -> Aff Unit
    loop = tailRecM \field -> do
      s <- map String.trim $ ReadLineAff.question "" interface
      if String.null s then
        if Array.any (Field.isPuttingAllowed field) $ allMoves width height then
          liftEffect $ Exception.throwException $ Exception.error "field is not fully occupied"
        else
          (liftEffect $ Console.info "") $> Loop initField
      else
        case String.split (String.Pattern " ") s of
          [ xStr, yStr ] ->
            let
              maybeNextField = do
                x <- Int.fromString xStr
                y <- Int.fromString yStr
                let pos = Tuple x y
                Field.putNextPoint pos field
            in
              Maybe.maybe'
                (\_ -> liftEffect $ Exception.throwException $ Exception.error "wrong pos")
                (\nextField -> (liftEffect $ Console.info $ (show $ Field.scoreRed nextField) <> " " <> (show $ Field.scoreBlack nextField)) $> Loop nextField)
                maybeNextField
          _ -> liftEffect $ Exception.throwException $ Exception.error "wrong input"
  Aff.launchAff_ $ loop initField
