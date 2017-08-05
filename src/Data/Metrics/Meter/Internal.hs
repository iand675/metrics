{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Metrics.Meter.Internal (
  Meter,
  meterData,
  mark,
  clear,
  tick,
  meanRate,
  oneMinuteAverage,
  fiveMinuteAverage,
  fifteenMinuteAverage,
  tickIfNecessary,
  count,
  lastTick
) where
import Control.Lens
import Control.Lens.TH
import Data.Time.Clock
import qualified Data.Metrics.MovingAverage as M

data Meter = Meter
  { meterCount             :: {-# UNPACK #-} !Int
  , meterOneMinuteRate     :: !M.MovingAverage
  , meterFiveMinuteRate    :: !M.MovingAverage
  , meterFifteenMinuteRate :: !M.MovingAverage
  , meterStartTime         :: !NominalDiffTime
  , meterLastTick          :: !NominalDiffTime
  , meterTickInterval      :: {-# UNPACK #-} !Double
  } deriving (Show)

makeFields ''Meter

meterData :: Double -> (Double -> Int -> M.MovingAverage) -> NominalDiffTime -> Meter
meterData ti f t = Meter
  { meterCount = 0
  , meterOneMinuteRate = f ti 1
  , meterFiveMinuteRate = f ti 5
  , meterFifteenMinuteRate = f ti 15
  , meterStartTime = t
  , meterLastTick = t
  , meterTickInterval = ti
  }

-- TODO: make moving average prism

mark :: NominalDiffTime -> Int -> Meter -> Meter
mark t c m = ticked
  & count +~ c
  & oneMinuteRate %~ updateMeter
  & fiveMinuteRate %~ updateMeter
  & fifteenMinuteRate %~ updateMeter
  where
    updateMeter = M.update $ fromIntegral c
    ticked = tickIfNecessary t m
{-# INLINEABLE mark #-}

clear :: NominalDiffTime -> Meter -> Meter
clear t =
  (count .~ 0) .
  (startTime .~ t) .
  (lastTick .~ t) .
  (oneMinuteRate %~ M.clear) .
  (fiveMinuteRate %~ M.clear) .
  (fifteenMinuteRate %~ M.clear)
{-# INLINEABLE clear #-}

tick :: Meter -> Meter
tick = (oneMinuteRate %~ M.tick) . (fiveMinuteRate %~ M.tick) . (fifteenMinuteRate %~ M.tick)
{-# INLINEABLE tick #-}

tickIfNecessary :: NominalDiffTime -> Meter -> Meter
tickIfNecessary new d = if age >= meterTickInterval d
  then iterate tick (d { meterLastTick = latest }) !! truncate (age / meterTickInterval d)
  else d
  where
    age = realToFrac (new - meterLastTick d)
    swapped = meterLastTick d < new
    latest = Prelude.max (meterLastTick d) new
{-# INLINEABLE tickIfNecessary #-}

meanRate :: NominalDiffTime -> Meter -> Double
meanRate t d = if c == 0
  then 0
  else fromIntegral c / elapsed
  where
    c = meterCount d
    start = meterStartTime d
    elapsed = realToFrac (t - start)
{-# INLINEABLE meanRate #-}

oneMinuteAverage :: Meter -> M.MovingAverage
oneMinuteAverage = meterOneMinuteRate

fiveMinuteAverage :: Meter -> M.MovingAverage
fiveMinuteAverage = meterFiveMinuteRate

fifteenMinuteAverage :: Meter -> M.MovingAverage
fifteenMinuteAverage = meterFifteenMinuteRate

