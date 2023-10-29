module Main where

import Test.Hspec
import Test.Tasty qualified
import Test.Tasty.Hspec

import Sim.Examples qualified as SimExamplesTests
import Sim.T21 qualified as SimT21Tests

main :: IO ()
main = do
  test <- testSpec "tis100" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec = parallel $ do
  SimT21Tests.simTestsSpec
  SimExamplesTests.simTestsSpec
