{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances   #-}
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
import Control.Monad.Base
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
  { fromTimer    :: !(MutVar (PrimState m) P.Timer)
  -- ^ A reference to the pure timer internals
  , timerGetTime :: !(m NominalDiffTime)
  -- ^ The function that provides time differences for the timer. In practice, this is usually just "getPOSIXTime"
  }

makeFields ''Timer

instance (MonadBase b m, PrimMonad b) => Clear b m (Timer b) where
  clear t = liftBase $ do
    ts <- timerGetTime t
    updateRef (fromTimer t) $ P.clear ts

instance (MonadBase b m, PrimMonad b) => Update b m (Timer b) Double where
  update t x = liftBase $ do
    ts <- timerGetTime t
    updateRef (fromTimer t) $ P.update ts x

instance (MonadBase b m, PrimMonad b) => Count b m (Timer b) where
  count t = liftBase $ fmap P.count $ readMutVar (fromTimer t)

instance (MonadBase b m, PrimMonad b) => Statistics b m (Timer b) where
  mean t = liftBase $ applyWithRef (fromTimer t) P.mean
  stddev t = liftBase $ applyWithRef (fromTimer t) P.stddev
  variance t = liftBase $ applyWithRef (fromTimer t) P.variance
  maxVal t = liftBase $ P.maxVal <$> readMutVar (fromTimer t)
  minVal t = liftBase $ P.minVal <$> readMutVar (fromTimer t)

instance (MonadBase b m, PrimMonad b) => Rate b m (Timer b) where
  oneMinuteRate t = liftBase $ do
    ts <- timerGetTime t
    updateAndApplyToRef (fromTimer t) (P.tickIfNecessary ts) P.oneMinuteRate
  fiveMinuteRate t = liftBase $ do
    ts <- timerGetTime t
    updateAndApplyToRef (fromTimer t) (P.tickIfNecessary ts) P.fiveMinuteRate
  fifteenMinuteRate t = liftBase $ do
    ts <- timerGetTime t
    updateAndApplyToRef (fromTimer t) (P.tickIfNecessary ts) P.fifteenMinuteRate
  meanRate t = liftBase $ do
    ts <- timerGetTime t
    applyWithRef (fromTimer t) (P.meanRate ts)

instance (MonadBase b m, PrimMonad b) => TakeSnapshot b m (Timer b) where
  snapshot t = liftBase $ applyWithRef (fromTimer t) P.snapshot

-- | Create a timer using a custom function for retrieving the current time.
--
-- This is mostly exposed for testing purposes: prefer using "timer" if possible.
mkTimer :: (MonadBase b m, PrimMonad b) => b NominalDiffTime -> Seed -> m (Timer b)
mkTimer mt s = liftBase $ do
  t <- mt
  let ewmaMeter = M.meterData 5 E.movingAverage t
  let histogram = H.histogram $ R.reservoir 0.015 1028 t s
  v <- newMutVar $ P.Timer ewmaMeter histogram
  return $ Timer v mt

-- | Create a standard "Timer" with an 
-- exponentially weighted moving average
-- and an exponentially decaying histogram
timer :: MonadBase IO m => m (Timer IO)
timer = liftBase $ do
  s <- withSystemRandom (asGenIO $ save)
  mkTimer getPOSIXTime s

-- | Execute an action and record statistics about the
-- duration of the event and the rate of event occurrence.
time :: MonadBase IO m => Timer IO -> m a -> m a
time t m = do
  let gt = t ^. getTime
  ts <- liftBase gt
  r <- m
  tf <- liftBase gt
  update t $ realToFrac $ tf - ts
  return r

