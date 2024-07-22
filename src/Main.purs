module Main where

import Prelude
import FieldComponent (fieldComponent)

import Data.List.NonEmpty as NonEmptyList
import Effect (Effect)
import Field as Field
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    runUI fieldComponent (NonEmptyList.singleton $ Field.emptyField 10 10) body
