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
  { _meterCount :: !Int
  , _meterOneMinuteRate :: !M.MovingAverage
  , _meterFiveMinuteRate :: !M.MovingAverage
  , _meterFifteenMinuteRate :: !M.MovingAverage
  , _meterStartTime :: !NominalDiffTime
  , _meterLastTick :: !NominalDiffTime
  }

makeFields ''Meter

meterData :: (Int -> M.MovingAverage) -> NominalDiffTime -> Meter
meterData f t = Meter
  { _meterCount = 0
  , _meterOneMinuteRate = f 1
  , _meterFiveMinuteRate = f 5
  , _meterFifteenMinuteRate = f 15
  , _meterStartTime = t
  , _meterLastTick = t
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

clear :: NominalDiffTime -> Meter -> Meter
clear t =
  (count .~ 0) .
  (startTime .~ t) .
  (lastTick .~ t) .
  (oneMinuteRate %~ M.clear) .
  (fiveMinuteRate %~ M.clear) .
  (fifteenMinuteRate %~ M.clear)

tick :: Meter -> Meter
tick = (oneMinuteRate %~ M.tick) . (fiveMinuteRate %~ M.tick) . (fifteenMinuteRate %~ M.tick)

tickIfNecessary :: NominalDiffTime -> Meter -> Meter
tickIfNecessary new d = if age >= 5
  then iterate tick (d { _meterLastTick = latest }) !! (truncate age `div` 5)
  else d
  where
    age = new - _meterLastTick d
    swapped = _meterLastTick d < new
    latest = Prelude.max (_meterLastTick d) new

meanRate :: NominalDiffTime -> Meter -> Double
meanRate t d = if c == 0
  then 0
  else fromIntegral c / fromIntegral elapsed
  where
    c = _meterCount d
    start = _meterStartTime d
    elapsed = fromEnum t - fromEnum start

oneMinuteAverage :: Meter -> M.MovingAverage
oneMinuteAverage = _meterOneMinuteRate

fiveMinuteAverage :: Meter -> M.MovingAverage
fiveMinuteAverage = _meterFiveMinuteRate

fifteenMinuteAverage :: Meter -> M.MovingAverage
fifteenMinuteAverage = _meterFifteenMinuteRate
