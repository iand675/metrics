{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- | A timer is essentially just a data type that combines
-- a "Meter" and a "Histogram" to track both the rate at which
-- events are triggered as well as timing statistics about the calls.
--
-- This module exports the pure internals, relying on the stateful version
-- to supply the pure timer with measurements.
module Data.Metrics.Timer.Internal where
import Control.Lens
import Data.Time.Clock
import qualified Data.Metrics.Histogram.Internal as H
import qualified Data.Metrics.MovingAverage as A
import qualified Data.Metrics.Meter.Internal as M
import qualified Data.Metrics.Snapshot as S

data Timer = Timer
  { timerMeter :: !M.Meter
  , timerHistogram :: !H.Histogram
  }

makeFields ''Timer

tickIfNecessary :: NominalDiffTime -> Timer -> Timer
tickIfNecessary t = meter %~ M.tickIfNecessary t

snapshot :: Timer -> S.Snapshot
snapshot = H.snapshot . timerHistogram

oneMinuteRate :: Timer -> Double
oneMinuteRate = A.rate . M.oneMinuteAverage . timerMeter

fiveMinuteRate :: Timer -> Double
fiveMinuteRate = A.rate . M.fiveMinuteAverage . timerMeter

fifteenMinuteRate :: Timer -> Double
fifteenMinuteRate = A.rate . M.fifteenMinuteAverage . timerMeter

meanRate :: NominalDiffTime -> Timer -> Double
meanRate t = M.meanRate t . timerMeter

count :: Timer -> Int
count = H.count . view histogram

clear :: NominalDiffTime -> Timer -> Timer
clear t = (histogram %~ H.clear t) . (meter %~ M.clear t)

update :: NominalDiffTime -> Double -> Timer -> Timer
update t x = (histogram %~ H.update x t) . (meter %~ M.mark t 1)

mean :: Timer -> Double
mean = H.mean . timerHistogram

stddev :: Timer -> Double
stddev = H.stddev . timerHistogram

variance :: Timer -> Double
variance = H.variance . timerHistogram

maxVal :: Timer -> Double
maxVal = H.maxVal . timerHistogram

minVal :: Timer -> Double
minVal = H.minVal . timerHistogram

