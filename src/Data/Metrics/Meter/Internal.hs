{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
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
  count
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
  }

makeFields ''Meter

meterData :: (Int -> M.MovingAverage) -> NominalDiffTime -> Meter
meterData f t = Meter
  { meterCount = 0
  , meterOneMinuteRate = f 1
  , meterFiveMinuteRate = f 5
  , meterFifteenMinuteRate = f 15
  , meterStartTime = t
  , meterLastTick = t
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
tickIfNecessary new d = if age >= 5
  then iterate tick (d { meterLastTick = latest }) !! (truncate age `div` 5)
  else d
  where
    age = new - meterLastTick d
    swapped = meterLastTick d < new
    latest = Prelude.max (meterLastTick d) new
{-# INLINEABLE tickIfNecessary #-}

meanRate :: NominalDiffTime -> Meter -> Double
meanRate t d = if c == 0
  then 0
  else fromIntegral c / fromIntegral elapsed
  where
    c = meterCount d
    start = meterStartTime d
    elapsed = fromEnum t - fromEnum start
{-# INLINEABLE meanRate #-}

oneMinuteAverage :: Meter -> M.MovingAverage
oneMinuteAverage = meterOneMinuteRate

fiveMinuteAverage :: Meter -> M.MovingAverage
fiveMinuteAverage = meterFiveMinuteRate

fifteenMinuteAverage :: Meter -> M.MovingAverage
fifteenMinuteAverage = meterFifteenMinuteRate

