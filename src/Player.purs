module Player where

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Prelude

data Player = Red | Black

derive instance Generic Player _

derive instance Eq Player

instance Show Player where
  show = genericShow

nextPlayer :: Player -> Player
nextPlayer Red = Black
nextPlayer Black = Red
