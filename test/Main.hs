module Main where

import TIS100
import Test.Hspec
import Test.Tasty qualified
import Test.Tasty.Hspec

import Sim.Tests qualified as SimTests

main :: IO ()
main = do
  test <- testSpec "tis100" spec
  Test.Tasty.defaultMain test

spec :: Spec
spec = SimTests.simTestsSpec
