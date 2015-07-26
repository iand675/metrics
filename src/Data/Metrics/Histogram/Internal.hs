{-# LANGUAGE BangPatterns #-}
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
  { histogramReservoir :: !R.Reservoir
  , histogramCount     :: {-# UNPACK #-} !Int
  , histogramMinVal    :: {-# UNPACK #-} !Double
  , histogramMaxVal    :: {-# UNPACK #-} !Double
  , histogramSum       :: {-# UNPACK #-} !Double
  , histogramVariance  :: {-# UNPACK #-} !(Double, Double)
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
      { histogramReservoir = R.clear t $ histogramReservoir s
      , histogramCount = 0
      , histogramMinVal = nan
      , histogramMaxVal = nan
      , histogramSum = 0
      , histogramVariance = (-1, 0)
      }
{-# INLINEABLE clear #-}

-- | Update statistics and the reservoir with a new sample.
update :: Double -> NominalDiffTime -> Histogram -> Histogram
update = go
  where
    go v t s = s
      { histogramReservoir = updatedReservoir
      , histogramCount = updatedCount
      , histogramMinVal = updateMin (histogramMinVal s) v
      , histogramMaxVal = updateMax (histogramMaxVal s) v
      , histogramSum = histogramSum s + v
      , histogramVariance = updateVariance updatedCount v $ histogramVariance s
      }
      where 
        updatedCount = succ $ histogramCount s
        updatedReservoir = R.update v t $ histogramReservoir s
{-# INLINEABLE update #-}

updateMin :: Double -> Double -> Double
updateMin ox x = if isNaN ox || ox > x then x else ox
{-# INLINE updateMin #-}

updateMax :: Double -> Double -> Double
updateMax ox x = if isNaN ox || ox < x then x else ox
{-# INLINE updateMax #-}

-- | Get the average of all samples since the histogram was created.
mean :: Histogram -> Double
mean = go
  where
    go s = if histogramCount s > 0
      then histogramSum s / fromIntegral (histogramCount s)
      else 0
{-# INLINEABLE mean #-}

-- | Get the standard deviation of all samples.
stddev :: Histogram -> Double
stddev = go
  where
    go s = if c > 0
      then (calculateVariance c $ snd $ histogramVariance s) ** 0.5
      else 0
      where c = histogramCount s
{-# INLINEABLE stddev #-}

-- | Get the variance of all samples.
variance :: Histogram -> Double
variance = go
  where
    go s = if c <= 1
      then 0
      else calculateVariance c $ snd $ histogramVariance s
      where c = histogramCount s
{-# INLINEABLE variance #-}

-- | Get the minimum value of all samples.
minVal :: Histogram -> Double
minVal = histogramMinVal

-- | Get the maximum value of all samples
maxVal :: Histogram -> Double
maxVal = histogramMaxVal

-- | Get the number of samples that the histogram has been updated with.
count :: Histogram -> Int
count = histogramCount

-- | Get a snapshot of the current reservoir's samples.
snapshot :: Histogram -> Snapshot
snapshot = R.snapshot . histogramReservoir
{-# INLINEABLE snapshot #-}

calculateVariance :: Int -> Double -> Double
calculateVariance c v = if c <= 1 then 0 else v / (fromIntegral c - 1)
{-# INLINEABLE calculateVariance #-}

updateVariance :: Int -> Double -> (Double, Double) -> (Double, Double)
updateVariance _ !c (-1, y) = (c, 0)
updateVariance count c (x, y) = (l, r)
  where
    c' = fromIntegral count
    diff = c - x
    !l = x + diff / c'
    !r = y + diff * (c - l)
{-# INLINEABLE updateVariance #-}

