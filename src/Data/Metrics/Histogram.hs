{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
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
  uniformSampler,
  module Data.Metrics.Types
) where
import           Control.Monad.Base
import           Control.Monad.Primitive
import qualified Data.Metrics.Histogram.Internal as P
import           Data.Metrics.Internal
import           Data.Metrics.Types
import           Data.Metrics.Reservoir (Reservoir)
import           Data.Metrics.Reservoir.Uniform (unsafeReservoir)
import           Data.Metrics.Reservoir.ExponentiallyDecaying (reservoir)
import           Data.Primitive.MutVar
import           Data.Time.Clock
import           Data.Time.Clock.POSIX
import           System.Random.MWC

-- | A measure of the distribution of values in a stream of data.
data Histogram m = Histogram
  { fromHistogram       :: !(MV m P.Histogram)
  , histogramGetSeconds :: !(m NominalDiffTime)
  }

instance (MonadBase b m, PrimMonad b) => Clear b m (Histogram b) where
  clear h = liftBase $ do
    t <- histogramGetSeconds h
    updateRef (fromHistogram h) $ P.clear t
  {-# INLINEABLE clear #-}

instance (MonadBase b m, PrimMonad b) => Update b m (Histogram b) Double where
  update h x = liftBase $ do
    t <- histogramGetSeconds h
    updateRef (fromHistogram h) $ P.update x t
  {-# INLINEABLE update #-}

instance (MonadBase b m, PrimMonad b) => Count b m (Histogram b) where
  count h = liftBase $ fmap P.count $ readMutVar (fromHistogram h)
  {-# INLINEABLE count #-}

instance (MonadBase b m, PrimMonad b) => Statistics b m (Histogram b) where
  mean h = liftBase $ applyWithRef (fromHistogram h) P.mean
  {-# INLINEABLE mean #-}

  stddev h = liftBase $ applyWithRef (fromHistogram h) P.stddev
  {-# INLINEABLE stddev #-}

  variance h = liftBase $ applyWithRef (fromHistogram h) P.variance
  {-# INLINEABLE variance #-}

  maxVal h = liftBase $ fmap P.maxVal $ readMutVar (fromHistogram h)
  {-# INLINEABLE maxVal #-}

  minVal h = liftBase $ fmap P.minVal $ readMutVar (fromHistogram h)
  {-# INLINEABLE minVal #-}

instance (MonadBase b m, PrimMonad b) => TakeSnapshot b m (Histogram b) where
  snapshot h = liftBase $ applyWithRef (fromHistogram h) P.snapshot
  {-# INLINEABLE snapshot #-}

-- | Create a histogram using a custom time data supplier function and a custom reservoir.
histogram :: (MonadBase b m, PrimMonad b) => b NominalDiffTime -> Reservoir -> m (Histogram b)
histogram t r = do
  v <- liftBase $ newMutVar $ P.histogram r
  return $ Histogram v t

-- | A histogram that gives all entries an equal likelihood of being evicted.
--
-- Probably not what you want for most time-series data.
uniformHistogram :: MonadBase IO m => Seed -> m (Histogram IO)
uniformHistogram s = liftBase $ histogram getPOSIXTime $ unsafeReservoir s 1028

-- | The recommended histogram type. It provides a fast histogram that
-- probabilistically evicts older entries using a weighting system. This
-- ensures that snapshots remain relatively fresh.
exponentiallyDecayingHistogram :: MonadBase IO m => m (Histogram IO)
exponentiallyDecayingHistogram = liftBase $ do
  t <- getPOSIXTime
  s <- createSystemRandom >>= save
  histogram getPOSIXTime $ reservoir 0.015 1028 t s

uniformSampler :: Seed -> P.Histogram
uniformSampler s = P.histogram (unsafeReservoir s 1028)

