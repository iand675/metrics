module EDR where
import Control.Monad.ST
import Data.Metrics.Reservoir
import Data.Metrics.Reservoir.ExponentiallyDecaying (reservoir)
import System.Random.MWC
import Test.QuickCheck

edrTests = [reservoirSizeIsLimited]

seed = runST (create >>= save)
testReservoir = testReservoir' 1028
testReservoir' size = reservoir 0.015 size 0 seed

reservoirSizeIsLimited :: Property
reservoirSizeIsLimited = forAll (choose (1, 5000)) $ \x ->
  forAll (choose (0, x * 2)) $ \y ->
    size ((iterate (update 1 1) $ testReservoir' x) !! y) <= x
