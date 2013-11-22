{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
-- | A meter measures the rate at which a set of events occur:
--
-- Meters measure the rate of the events in a few different ways. The mean rate is the average rate of events. It’s generally useful for trivia, but as it represents the total rate for your application’s entire lifetime (e.g., the total number of requests handled, divided by the number of seconds the process has been running), it doesn’t offer a sense of recency. Luckily, meters also record three different exponentially-weighted moving average rates: the 1-, 5-, and 15-minute moving averages.
--
-- (Just like the Unix load averages visible in uptime or top.)
module Data.Metrics.Meter (
  Meter,
  meter,
  mark,
  mark',
  module Data.Metrics.Types
) where
import Control.Lens
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
  { fromMeter :: !(MV m P.Meter)
  , meterGetSeconds :: !(m NominalDiffTime)
  }

instance PrimMonad m => Rate m (Meter m) where
  oneMinuteRate m = do
    t <- meterGetSeconds m
    updateAndApplyToRef (fromMeter m) (P.tickIfNecessary t) (A.rate . P.oneMinuteAverage)
  fiveMinuteRate m = do
    t <- meterGetSeconds m
    updateAndApplyToRef (fromMeter m) (P.tickIfNecessary t) (A.rate . P.fiveMinuteAverage)
  fifteenMinuteRate m = do
    t <- meterGetSeconds m
    updateAndApplyToRef (fromMeter m) (P.tickIfNecessary t) (A.rate . P.fifteenMinuteAverage)
  meanRate m = do
    t <- meterGetSeconds m
    applyWithRef (fromMeter m) $ P.meanRate t

instance PrimMonad m => Count m (Meter m) where
  count m = readMutVar (fromMeter m) >>= return . view P.count

-- | Register multiple occurrences of an event.
mark' :: PrimMonad m => Meter m -> Int -> m ()
mark' m x = do
  t <- meterGetSeconds m
  updateRef (fromMeter m) (P.mark t x)

-- | Register a single occurrence of an event.
mark :: PrimMonad m => Meter m -> m ()
mark = flip mark' 1

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

