module GaugeTest where
import Data.Metrics.Gauge
import Data.Metrics.Types
import Test.QuickCheck
import Test.QuickCheck.Monadic

gaugeTests :: [Property]
gaugeTests = [ testValue {-- , testSet, testRatio --} ]

testValue :: Property
testValue = label "test retrieving gauge value" $ monadicIO $ do
  x <- pick arbitrary
  x' <- run $ do
    g <- gauge $ return x
    value g
  assert $ x == x'

testSet :: Property
testSet = label "test overwriting gauge action" $ monadicIO $ do
  x <- pick arbitrary
  y <- pick arbitrary
  z <- run $ do
    g <- gauge $ return x
    set g $ return y
    value g
  assert $ y == z

testRatio :: Property
testRatio = label "test creating a ratio gauge" $ monadicIO $ do
  x <- pick arbitrary
  (NonZero y) <- pick arbitrary
  z <- run $ do
    g <- gauge $ ratio (return x) (return y)
    value g
  assert $ z == (x / y)
