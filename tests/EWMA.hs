module EWMA where
import Data.Metrics.Internal
import Data.Metrics.MovingAverage.ExponentiallyWeighted
import Test.QuickCheck

ewmaTests =
  [
  {- property ticksDecreaseRateToZero
  , property ticksUpdateRate
  , constantRates
  , clearResetsRate
  -}
  ]

smallNumbers = choose (1, 10000)

ticksDecreaseRateToZero :: NonZero Double -> Property
ticksDecreaseRateToZero (NonZero x) = label "ticks decay rate towards 0" $ forAll smallNumbers $ \tc ->
  abs (rate (run !! tc)) < abs x
  where
    run = iterate tick $ update x $ empty 5 1

deltaEq :: Double -> Double -> Double -> Bool
deltaEq range x y = abs (x - y) <= range

ticksUpdateRate :: NonZero Double -> Property
ticksUpdateRate (NonZero x) = label "updating rate and ticking once returns current rate" $ deltaEq 0.005 (rate run) x
  where
    run = tick $ update x $ empty 5 1

constantRates :: Property
constantRates = label "constant rates" $ \(NonZero x) -> forAll smallNumbers $ \t ->
   deltaEq 0.005 x $ rate (iterate (tick . update x) (empty 5 1) !! t)

clearResetsRate :: Property
clearResetsRate = label "reset rate" $ property (clearedRate == 0)
  where
    clearedRate = rate $ clear $ updatedRate
    updatedRate = tick $ update 10 $ empty 5 1
