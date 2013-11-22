{-# LANGUAGE FlexibleInstances #-}
-- | An interface for bundling metrics in a way that they cna be iterated over for reporting or looked up for use by code that shares the registry.
module Data.Metrics.Registry (
  MetricRegistry,
  Metric(..),
  Register(..),
  module Data.Metrics.Types
) where
import Control.Concurrent.MVar
import qualified Data.HashMap.Strict as H
import Data.Metrics.Counter
import Data.Metrics.Gauge
import Data.Metrics.Histogram
import Data.Metrics.Meter
import Data.Metrics.Timer
import Data.Metrics.Types
import Data.Text (Text)

-- | A container that tracks all metrics registered with it.
-- All forms of metrics share the same namespace in the registry.
-- Consequently, attempting to replace a metric with one of a different type will fail (return Nothing from a call to `register`).
data MetricRegistry m = MetricRegistry
  { metrics :: !(MVar (H.HashMap Text (Metric m)))
  }

-- | A sum type of all supported metric types that reporters should be able to output.
data Metric m
  = MetricGauge !(Gauge m)
  | MetricCounter !(Counter m)
  | MetricHistogram !(Histogram m)
  | MetricMeter !(Meter m)
  | MetricTimer !(Timer m)

-- | Add a new metric to a registry or retrieve the existing metric of the same name if one exists.
class Register a where
  -- | If possible, avoid using 'register' to frequently retrieve metrics from a global registry. The metric registry is locked any time a lookup is performed, which may cause contention.
  register :: MetricRegistry IO -> Text -> IO a -> IO (Maybe a)

instance Register (Counter IO) where
  register r t m = do
    hm <- takeMVar $ metrics r
    case H.lookup t hm of
      Nothing -> do
        c <- m
        putMVar (metrics r) $! H.insert t (MetricCounter c) hm
        return $! Just c
      Just im -> do
        putMVar (metrics r) hm
        return $! case im of
          MetricCounter c -> Just c
          _ -> Nothing

instance Register (Gauge IO) where
  register r t m = do
    hm <- takeMVar $ metrics r
    case H.lookup t hm of
      Nothing -> do
        g <- m
        putMVar (metrics r) $! H.insert t (MetricGauge g) hm
        return $! Just g
      Just im -> do
        putMVar (metrics r) hm
        return $! case im of
          MetricGauge r -> Just r
          _ -> Nothing

instance Register (Histogram IO) where
  register r t m = do
    hm <- takeMVar $ metrics r
    case H.lookup t hm of
      Nothing -> do
        h <- m
        putMVar (metrics r) $! H.insert t (MetricHistogram h) hm
        return $! Just h
      Just im -> do
        putMVar (metrics r) hm
        return $! case im of
          MetricHistogram h -> Just h
          _ -> Nothing

instance Register (Meter IO) where
  register r t m = do
    hm <- takeMVar $ metrics r
    case H.lookup t hm of
      Nothing -> do
        mv <- m
        putMVar (metrics r) $! H.insert t (MetricMeter mv) hm
        return $! Just mv
      Just im -> do
        putMVar (metrics r) hm
        return $! case im of
          MetricMeter md -> Just md
          _ -> Nothing

instance Register (Timer IO) where
  register r t m = do
    hm <- takeMVar $ metrics r
    case H.lookup t hm of
      Nothing -> do
        mv <- m
        putMVar (metrics r) $! H.insert t (MetricTimer mv) hm
        return $! Just mv
      Just im -> do
        putMVar (metrics r) hm
        return $! case im of
          MetricTimer md -> Just md
          _ -> Nothing
