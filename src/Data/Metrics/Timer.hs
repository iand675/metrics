{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
-- | A timer is basically a histogram of the duration of a type of event and a meter of the rate of its occurrence.
module Data.Metrics.Timer (
  Timer,
  mkTimer,
  timer,
  time,
  module Data.Metrics.Types
) where
import Control.Applicative
import Control.Lens
import Control.Lens.TH
import Control.Monad.Primitive
import qualified Data.Metrics.MovingAverage.ExponentiallyWeighted as E
import qualified Data.Metrics.Histogram.Internal as H
import qualified Data.Metrics.Meter.Internal as M
import qualified Data.Metrics.Timer.Internal as P
import qualified Data.Metrics.Reservoir.ExponentiallyDecaying as R
import Data.Metrics.Internal
import Data.Metrics.Types
import Data.Primitive.MutVar
import Data.Time.Clock
import Data.Time.Clock.POSIX
import System.Random.MWC

-- | A measure of time statistics for the duration of an event
data Timer m = Timer
  { fromTimer :: !(MutVar (PrimState m) P.Timer) -- ^ A reference to the pure timer internals
  , _timerGetTime :: !(m NominalDiffTime) -- ^ The function that provides time differences for the timer. In practice, this is usually just "getPOSIXTime"
  }

makeFields ''Timer

instance PrimMonad m => Clear m (Timer m) where
  clear t = do
    ts <- _timerGetTime t
    updateRef (fromTimer t) $ P.clear ts

instance PrimMonad m => Update m (Timer m) Double where
  update t x = do
    ts <- _timerGetTime t
    updateRef (fromTimer t) $ P.update ts x

instance PrimMonad m => Count m (Timer m) where
  count t = readMutVar (fromTimer t) >>= return . P.count

instance (Functor m, PrimMonad m) => Statistics m (Timer m) where
  mean t = applyWithRef (fromTimer t) P.mean
  stddev t = applyWithRef (fromTimer t) P.stddev
  variance t = applyWithRef (fromTimer t) P.variance
  maxVal t = P.maxVal <$> readMutVar (fromTimer t)
  minVal t = P.minVal <$> readMutVar (fromTimer t)

instance PrimMonad m => Rate m (Timer m) where
  oneMinuteRate t = do
    ts <- _timerGetTime t
    updateAndApplyToRef (fromTimer t) (P.tickIfNecessary ts) P.oneMinuteRate
  fiveMinuteRate t = do
    ts <- _timerGetTime t
    updateAndApplyToRef (fromTimer t) (P.tickIfNecessary ts) P.fiveMinuteRate
  fifteenMinuteRate t = do
    ts <- _timerGetTime t
    updateAndApplyToRef (fromTimer t) (P.tickIfNecessary ts) P.fifteenMinuteRate
  meanRate t = do
    ts <- _timerGetTime t
    applyWithRef (fromTimer t) (P.meanRate ts)

instance PrimMonad m => TakeSnapshot m (Timer m) where
  snapshot t = applyWithRef (fromTimer t) P.snapshot

-- | Create a timer using a custom function for retrieving the current time.
--
-- This is mostly exposed for testing purposes: prefer using "timer" if possible.
mkTimer :: PrimMonad m => m NominalDiffTime -> Seed -> m (Timer m)
mkTimer mt s = do
  t <- mt
  let ewmaMeter = M.meterData (E.movingAverage 5) t
  let histogram = H.histogram $ R.reservoir 0.015 1028 t s
  v <- newMutVar $ P.Timer ewmaMeter histogram
  return $ Timer v mt

-- | Create a standard "Timer" with an 
-- exponentially weighted moving average
-- and an exponentially decaying histogram
timer :: IO (Timer IO)
timer = do
  s <- withSystemRandom (asGenIO $ save)
  mkTimer getPOSIXTime s

-- | Execute an action and record statistics about the
-- duration of the event and the rate of event occurrence.
time :: Timer IO -> IO a -> IO a
time t m = do
  let gt = t ^. getTime
  ts <- gt
  r <- m
  tf <- gt
  update t $ realToFrac $ tf - ts
  return r

