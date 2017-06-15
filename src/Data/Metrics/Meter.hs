{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | A meter measures the rate at which a set of events occur:
--
-- Meters measure the rate of the events in a few different ways. The mean rate is the average rate of events. It’s generally useful for trivia, but as it represents the total rate for your application’s entire lifetime (e.g., the total number of requests handled, divided by the number of seconds the process has been running), it doesn’t offer a sense of recency. Luckily, meters also record three different exponentially-weighted moving average rates: the 1-, 5-, and 15-minute moving averages.
--
-- (Just like the Unix load averages visible in uptime or top.)
--
-- Meters are instances of the 'Rate' type-class, which provides the
-- methods 'oneMinuteRate', 'fiveMinuteRate', 'fifteenMinuteRate' and
-- 'meanRate' for retrieving the different rates from a meter.
--
-- Note that the methods 'oneMinuteRate', 'fiveMinuteRate' and
-- 'fifteenMinuteRate' provide the respective exponentially weighted
-- moving averages of rates as number of events per five second
-- timeframe. Therefore, in order to obtain these rates as number of
-- events per second, they need to be scaled by @0.2@.
--
-- On the other hand, the method 'meanRate' provides the mean rate as
-- total number of events per picosecond. Thus, scaling this number by
-- @10^12@ yields the mean rate as number of events per second.
module Data.Metrics.Meter (
  Meter,
  meter,
  mark,
  mark',
  mkMeter,
  fromMeter,
  module Data.Metrics.Types
) where
import Control.Lens
import Control.Monad.Base
import Control.Monad.Primitive
import Data.Primitive.MutVar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.HashMap.Strict as H
import Data.Metrics.Internal
import qualified Data.Metrics.Meter.Internal as P
import qualified Data.Metrics.MovingAverage as A
import qualified Data.Metrics.MovingAverage.ExponentiallyWeighted as EWMA
import Data.Metrics.Types

-- | A measure of the /rate/ at which a set of events occurs.
data Meter m = Meter
  { fromMeter       :: !(MV m P.Meter)
  , meterGetSeconds :: !(m NominalDiffTime)
  }

instance (MonadBase b m, PrimMonad b) => Rate b m (Meter b) where
  oneMinuteRate m = liftBase $ do
    t <- meterGetSeconds m
    updateAndApplyToRef (fromMeter m) (P.tickIfNecessary t) (A.rate . P.oneMinuteAverage)
  {-# INLINEABLE oneMinuteRate #-}

  fiveMinuteRate m = liftBase $ do
    t <- meterGetSeconds m
    updateAndApplyToRef (fromMeter m) (P.tickIfNecessary t) (A.rate . P.fiveMinuteAverage)
  {-# INLINEABLE fiveMinuteRate #-}

  fifteenMinuteRate m = liftBase $ do
    t <- meterGetSeconds m
    updateAndApplyToRef (fromMeter m) (P.tickIfNecessary t) (A.rate . P.fifteenMinuteAverage)
  {-# INLINEABLE fifteenMinuteRate #-}

  meanRate m = liftBase $ do
    t <- meterGetSeconds m
    applyWithRef (fromMeter m) $ P.meanRate t
  {-# INLINEABLE meanRate #-}

instance (MonadBase b m, PrimMonad m) => Count b m (Meter m) where
  count = fmap (view P.count) . readMutVar . fromMeter
  {-# INLINEABLE count #-}

-- | Register multiple occurrences of an event.
mark' :: PrimMonad m => Meter m -> Int -> m ()
mark' m x = do
  t <- meterGetSeconds m
  updateRef (fromMeter m) (P.mark t x)
{-# INLINEABLE mark' #-}

-- | Register a single occurrence of an event.
mark :: PrimMonad m => Meter m -> m ()
mark = flip mark' 1
{-# INLINEABLE mark #-}

-- | Create a new meter using an exponentially weighted moving average
meter :: IO (Meter IO)
meter = mkMeter getPOSIXTime

-- | Create a meter using a custom function for retrieving the current time.
--
-- This is mostly exposed for testing purposes: prefer using "meter" if possible.
mkMeter :: PrimMonad m => m NominalDiffTime -> m (Meter m)
mkMeter m = do
  t <- m
  v <- newMutVar $ ewmaMeter t
  return $! Meter v m

-- | Make a pure meter using an exponentially weighted moving average
ewmaMeter :: NominalDiffTime -- ^ The starting time of the meter.
  -> P.Meter
ewmaMeter = P.meterData (EWMA.movingAverage 0.5)

