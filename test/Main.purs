module Test.Main where

import Prelude

import Data.Traversable (sequence_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Client.Auth (tests)


main :: Effect Unit
main = launchAff_ $ sequence_ tests
