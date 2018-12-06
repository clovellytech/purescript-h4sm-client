module Test.Main where

import Prelude

import Data.Traversable (traverse, traverse_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Console (log)
import Test.AuthClient (tests)


main :: Effect Unit
main = do
  traverse_ launchAff_ tests
