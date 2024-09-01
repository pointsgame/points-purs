module Main where

import Prelude

import ArgParse.Basic (ArgParser)
import ArgParse.Basic as ArgParse
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (class MonadTrans, lift)
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
import Node.Process (argv, exit')
import Pipes ((<-<), await)
import Pipes.Core (Consumer, Producer, runEffectRec)
import Pipes.Prelude as Pipes
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

randomGames :: forall t. MonadTrans t => Monad (t Gen) => Int -> Int -> Int -> Producer Field (t Gen) Unit
randomGames games width height = Pipes.replicateM games $ lift $ randomGame width height

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

summator :: forall m. Monad m => Consumer Result (WriterT Result m) Unit
summator = forever $ await >>= tell

main :: Effect Unit
main = do
  args <- argv
  case ArgParse.parseArgs "bench" "Field benchmark." argsParser (Array.drop 2 args) of
    Either.Left e -> do
      Console.log $ ArgParse.printArgError e
      exit' 1
    Either.Right (Args args') -> do
      let
        Result result = flip Gen.evalGen { newSeed: LCG.mkSeed 0, size: 0 }
          $ execWriterT <<< runEffectRec
          $ summator <-< Pipes.map gameResult <-< randomGames args'.gamesNumber args'.width args'.height
      Console.log $ show result.red <> ":" <> show result.black
