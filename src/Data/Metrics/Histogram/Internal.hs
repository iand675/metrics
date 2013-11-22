-- | The pure interface for histograms.
-- This module is typically not as useful as the stateful implementation
-- since reservoir updates require retrieving the current time.
module Data.Metrics.Histogram.Internal (
  Histogram,
  histogram,
  clear,
  update,
  mean,
  stddev,
  variance,
  minVal,
  maxVal,
  count,
  snapshot
) where
import Data.Time.Clock
import qualified Data.Metrics.Reservoir as R
import Data.Metrics.Snapshot (Snapshot)

-- | A pure histogram that maintains a bounded reservoir of samples and basic statistical data about the samples.
data Histogram = Histogram
  { _histogramReservoir :: !R.Reservoir
  , _histogramCount :: !Int
  , _histogramMinVal :: !Double
  , _histogramMaxVal :: !Double
  , _histogramSum :: !Double
  , _histogramVariance :: !(Double, Double)
  }

-- | Create a histogram using a custom reservoir.
histogram :: R.Reservoir -> Histogram
histogram r = Histogram r 0 nan nan 0 (0, 0)

nan :: Double
nan = 0 / 0

-- | Reset all statistics, in addition to the underlying reservoir.
clear :: NominalDiffTime -> Histogram -> Histogram
clear = go
  where
    go t s = s
      { _histogramReservoir = R.clear t $ _histogramReservoir s
      , _histogramCount = 0
      , _histogramMinVal = nan
      , _histogramMaxVal = nan
      , _histogramSum = 0
      , _histogramVariance = (-1, 0)
      }

-- | Update statistics and the reservoir with a new sample.
update :: Double -> NominalDiffTime -> Histogram -> Histogram
update = go
  where
    go v t s = s
      { _histogramReservoir = updatedReservoir
      , _histogramCount = updatedCount
      , _histogramMinVal = updateMin (_histogramMinVal s) v
      , _histogramMaxVal = updateMax (_histogramMaxVal s) v
      , _histogramSum = _histogramSum s + v
      , _histogramVariance = updateVariance updatedCount v $ _histogramVariance s
      }
      where 
        updatedCount = succ $ _histogramCount s
        updatedReservoir = R.update v t $ _histogramReservoir s

updateMin :: Double -> Double -> Double
updateMin ox x = if isNaN ox || ox > x then x else ox

updateMax :: Double -> Double -> Double
updateMax ox x = if isNaN ox || ox < x then x else ox

-- | Get the average of all samples since the histogram was created.
mean :: Histogram -> Double
mean = go
  where
    go s = if _histogramCount s > 0
      then _histogramSum s / fromIntegral (_histogramCount s)
      else 0

-- | Get the standard deviation of all samples.
stddev :: Histogram -> Double
stddev = go
  where
    go s = if c > 0
      then (calculateVariance c $ snd $ _histogramVariance s) ** 0.5
      else 0
      where c = _histogramCount s

-- | Get the variance of all samples.
variance :: Histogram -> Double
variance = go
  where
    go s = if c <= 1
      then 0
      else calculateVariance c $ snd $ _histogramVariance s
      where c = _histogramCount s

-- | Get the minimum value of all samples.
minVal :: Histogram -> Double
minVal = _histogramMinVal

-- | Get the maximum value of all samples
maxVal :: Histogram -> Double
maxVal = _histogramMaxVal

-- | Get the number of samples that the histogram has been updated with.
count :: Histogram -> Int
count = _histogramCount

-- | Get a snapshot of the current reservoir's samples.
snapshot :: Histogram -> Snapshot
snapshot = R.snapshot . _histogramReservoir

calculateVariance :: Int -> Double -> Double
calculateVariance c v = if c <= 1 then 0 else v / (fromIntegral c - 1)

updateVariance :: Int -> Double -> (Double, Double) -> (Double, Double)
updateVariance _ c (-1, y) = (c, 0)
updateVariance count c (x, y) = (l, r)
  where
    c' = fromIntegral count
    diff = c - x
    l = x + diff / c'
    r = y + diff * (c - l)
