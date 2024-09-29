module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParse
import Control.Monad.Rec.Class (Step(..), tailRecM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Writer (WriterT)
import Control.Monad.Writer.Trans (execWriterT, tell)
import Data.Array ((..))
import Data.Array as Array
import Data.Either as Either
import Data.Maybe as Maybe
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Class.Console as Console
import Field (Pos, Field)
import Field as Field
import Player as Player
import Random.LCG as LCG
import Test.QuickCheck.Gen (Gen)
import Test.QuickCheck.Gen as Gen

newtype Args = Args
  { width :: Int
  , height :: Int
  , gamesNumber :: Int
  , seed :: Int
  }

derive instance Newtype Args _

widthParser :: ArgParser Int
widthParser = ArgParse.int $ ArgParse.argument [ "--width", "-w" ] "Field width."

heightParser :: ArgParser Int
heightParser = ArgParse.int $ ArgParse.argument [ "--height", "-h" ] "Field height."

gamesNumberParser :: ArgParser Int
gamesNumberParser = ArgParse.int $ ArgParse.argument [ "--games-number", "-n" ] "Games number."

seedParser :: ArgParser Int
seedParser = ArgParse.int $ ArgParse.argument [ "--seed", "-s" ] "RNG seed."

argsParser :: ArgParser Args
argsParser = (\width height gamesNumber seed -> Args { width, height, gamesNumber, seed }) <$> widthParser <*> heightParser <*> gamesNumberParser <*> seedParser

allMoves :: Int -> Int -> Array Pos
allMoves width height = do
  x <- 0 .. (width - 1)
  y <- 0 .. (height - 1)
  pure $ Tuple x y

randomGame :: Int -> Int -> Gen Field
randomGame width height =
  Array.foldl (\field pos -> Maybe.fromMaybe field $ Field.putNextPoint pos field) (Field.emptyField width height) <$> Gen.shuffle (allMoves width height)

newtype Result = Result
  { red :: Int
  , black :: Int
  }

derive instance Newtype Result _

instance Semigroup Result where
  append (Result l) (Result r) = Result { red: l.red + r.red, black: l.black + r.black }

instance Monoid Result where
  mempty = Result { red: 0, black: 0 }

gameResult :: Field -> Result
gameResult field = case Field.winner field of
  Maybe.Just Player.Red -> Result { red: 1, black: 0 }
  Maybe.Just Player.Black -> Result { red: 0, black: 1 }
  Maybe.Nothing -> Result { red: 0, black: 0 }

randomGamesResult :: Int -> Int -> Int -> WriterT Result Gen Unit
randomGamesResult n width height = tailRecM go n
  where
  go 0 = pure $ Done unit
  go n' = do
    game <- lift $ randomGame width height
    tell $ gameResult game
    pure $ Loop $ n' - 1

foreign import exit :: forall a. Int -> Effect a
foreign import argv :: Effect (Array String)

main :: Effect Unit
main = do
  args <- argv
  case ArgParse.parseArgs "bench" "Field benchmark." argsParser (Array.drop 2 args) of
    Either.Left e -> do
      Console.log $ ArgParse.printArgError e
      exit 1
    Either.Right (Args args') -> do
      let
        Result result = flip Gen.evalGen { newSeed: LCG.mkSeed (args'.seed), size: 0 } $ execWriterT $ randomGamesResult args'.gamesNumber args'.width args'.height
      Console.log $ show result.red <> ":" <> show result.black
