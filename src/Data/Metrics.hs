{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Metrics
-- Copyright   : (c) Ian Duncan 2013
-- Stability   : experimental
-- Portability : non-portable
--
-- A library for tracking arbitrary metrics over time.
-- The library largely provides pure and stateful versions of
-- the same set of functionality for common metric types.
--
module Data.Metrics (
  module Data.HealthCheck,
  module Data.Metrics.Counter,
  module Data.Metrics.Gauge,
  module Data.Metrics.Histogram,
  module Data.Metrics.Meter,
  module Data.Metrics.Registry,
  module Data.Metrics.Timer,
  module Data.Metrics.Types
) where
import Data.HealthCheck
import Data.Metrics.Counter
import Data.Metrics.Gauge
import Data.Metrics.Histogram
import Data.Metrics.Meter
import Data.Metrics.Registry
import Data.Metrics.Timer
import Data.Metrics.Types

{-
getOrInit :: (Typeable a, MetricOutput a) => MetricRegistry -> Text -> a -> IO (Maybe (IORef a))
getOrInit r name conv defaultValue = do
  hm <- takeMVar $ metrics r
  case H.lookup name hm of
    Nothing -> do
      ref <- newIORef defaultValue
      let getRep = readIORef ref >>= conv
      putMVar (metrics r) $! H.insert name (getRep, toDyn ref) hm
      return $! Just ref
    Just (_, ref) -> do
      putMVar (metrics r) hm
      return $! fromDynamic ref
-}

{-
counter :: MonadIO m => MetricRegistry -> Text -> m (Maybe Counter)
counter registry conv name = do
  mref <- liftIO $ getOrInit registry name (return . conv) 0
  case mref of
    Nothing -> return Nothing
    Just ref -> return $! Just $! Counter ref

gauge :: Typeable a => MetricRegistry -> Text -> IO a -> IO (Maybe (Gauge a))
gauge registry conv name g = do
  mref <- liftIO $ getOrInit registry name (fmap conv) g
  case mref of
    Nothing -> return Nothing
    Just ref -> return $! Just $! Gauge ref

cache :: DiffTime -> Gauge a -> m ()
DerivativeGauge
ExponentiallyWeightedMovingAverage
ExponentiallyDecayingReservoir

data Ratio = Ratio
  { numerator :: IORef Double
  , denominator :: IORef Double
  }

newtype RatioGauge = Gauge Ratio
Reservoir
SlidingTimeWindowReservoir

cachedGauge
derivedGauge
-}

--test = do
--  r <- newMetricRegistry
--  (Just c) <- register r "wombats.sighted" counter
--  (Just c2) <- register r "wombats.sighted" counter
--  increment c
--  value c >>= print
--  value c2 >>= print

