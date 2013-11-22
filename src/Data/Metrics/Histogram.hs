{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- | Histogram metrics allow you to measure not just easy things like the min, mean, max, and standard deviation of values, but also quantiles like the median or 95th percentile.
--
-- Traditionally, the way the median (or any other quantile) is calculated is to take the entire data set, sort it, and take the value in the middle (or 1% from the end, for the 99th percentile). This works for small data sets, or batch processing systems, but not for high-throughput, low-latency services.
--
-- The solution for this is to sample the data as it goes through. By maintaining a small, manageable reservoir which is statistically representative of the data stream as a whole, we can quickly and easily calculate quantiles which are valid approximations of the actual quantiles. This technique is called reservoir sampling.
module Data.Metrics.Histogram (
  Histogram,
  histogram,
  exponentiallyDecayingHistogram,
  uniformHistogram,
  module Data.Metrics.Types
) where
import Control.Monad.Primitive
import qualified Data.Metrics.Histogram.Internal as P
import Data.Metrics.Internal
import Data.Metrics.Types
import Data.Metrics.Reservoir (Reservoir)
import Data.Metrics.Reservoir.Uniform (unsafeReservoir)
import Data.Metrics.Reservoir.ExponentiallyDecaying (reservoir)
import Data.Primitive.MutVar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import System.Random.MWC

-- | A measure of the distribution of values in a stream of data.
data Histogram m = Histogram
  { fromHistogram :: MV m P.Histogram
  , histogramGetSeconds :: m NominalDiffTime
  }

instance PrimMonad m => Clear m (Histogram m) where
  clear h = do
    t <- histogramGetSeconds h
    updateRef (fromHistogram h) $ P.clear t

instance PrimMonad m => Update m (Histogram m) Double where
  update h x = do
    t <- histogramGetSeconds h
    updateRef (fromHistogram h) $ P.update x t

instance PrimMonad m => Count m (Histogram m) where
  count h = readMutVar (fromHistogram h) >>= return . P.count

instance PrimMonad m => Statistics m (Histogram m) where
  mean h = applyWithRef (fromHistogram h) P.mean
  stddev h = applyWithRef (fromHistogram h) P.stddev
  variance h = applyWithRef (fromHistogram h) P.variance
  maxVal h = readMutVar (fromHistogram h) >>= return . P.maxVal
  minVal h = readMutVar (fromHistogram h) >>= return . P.minVal

instance PrimMonad m => TakeSnapshot m (Histogram m) where
  snapshot h = applyWithRef (fromHistogram h) P.snapshot

-- | Create a histogram using a custom time data supplier function and a custom reservoir.
histogram :: PrimMonad m => m NominalDiffTime -> Reservoir -> m (Histogram m)
histogram t r = do
  v <- newMutVar $ P.histogram r
  return $ Histogram v t

-- | A histogram that gives all entries an equal likelihood of being evicted.
--
-- Probably not what you want for most time-series data.
uniformHistogram :: Seed -> IO (Histogram IO)
uniformHistogram s = histogram getPOSIXTime $ unsafeReservoir s 1028

-- | The recommended histogram type. It provides a fast histogram that
-- probabilistically evicts older entries using a weighting system. This
-- ensures that snapshots remain relatively fresh.
exponentiallyDecayingHistogram :: IO (Histogram IO)
exponentiallyDecayingHistogram = do
  t <- getPOSIXTime
  s <- createSystemRandom >>= save
  histogram getPOSIXTime $ reservoir 0.015 1028 t s

uniformSampler :: Seed -> P.Histogram
uniformSampler s = P.histogram (unsafeReservoir s 1028)

nan :: Double
nan = 0 / 0
