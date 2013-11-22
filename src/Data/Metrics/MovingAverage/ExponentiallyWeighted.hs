{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
-- | An exponentially-weighted moving average.
--
-- see /UNIX Load Average Part 1: How It Works/:
--
-- <http://www.teamquest.com/pdfs/whitepaper/ldavg1.pdf>
--
-- see /UNIX Load Average Part 2: Not Your Average Average/
--
-- <http://www.teamquest.com/pdfs/whitepaper/ldavg2.pdf>
-- 
-- see Wikipedia's article on exponential moving averages:
--
-- <http://en.wikipedia.org/wiki/Moving_average#Exponential_moving_average>
module Data.Metrics.MovingAverage.ExponentiallyWeighted (
  ExponentiallyWeightedMovingAverage,
  new1MinuteMovingAverage,
  new5MinuteMovingAverage,
  new15MinuteMovingAverage,
  movingAverage,
  clear,
  rate,
  empty,
  update,
  tick
) where
import Control.Lens
import Control.Lens.TH
import Control.Monad.Primitive
import qualified Data.Metrics.MovingAverage as MA
import Data.Metrics.Types (Minutes)

-- | The internal representation of the exponentially weighted moving average.
--
-- This type encapsulates the state needed for the exponentially weighted "MovingAverage" implementation.
data ExponentiallyWeightedMovingAverage = ExponentiallyWeightedMovingAverage
  { _ewmaUncounted :: !Double
  , _ewmaCurrentRate :: !Double
  , _ewmaInitialized :: !Bool
  , _ewmaInterval :: !Double
  , _ewmaAlpha :: !Double
  } deriving (Show)

makeFields ''ExponentiallyWeightedMovingAverage

makeAlpha :: Double -> Minutes -> Double
makeAlpha i m = 1 - exp (negate i / 60 / fromIntegral m)

-- | Create a new "MovingAverage" with 5 second tick intervals for a one-minute window.
new1MinuteMovingAverage :: MA.MovingAverage
new1MinuteMovingAverage = movingAverage 5 1

-- | Create a new "MovingAverage" with 5 second tick intervals for a five-minute window.
new5MinuteMovingAverage :: MA.MovingAverage
new5MinuteMovingAverage = movingAverage 5 5

-- | Create a new "MovingAverage" with 5 second tick intervals for a fifteen-minute window.
new15MinuteMovingAverage :: MA.MovingAverage
new15MinuteMovingAverage = movingAverage 5 15

-- | Create a new "MovingAverage" with the given tick interval and averaging window.
movingAverage :: Double -> Minutes -> MA.MovingAverage
movingAverage i m = MA.MovingAverage
  { MA.movingAverageClear = clear
  , MA.movingAverageUpdate = update
  , MA.movingAverageTick = tick
  , MA.movingAverageRate = rate
  , MA.movingAverageState = empty i m
  }

-- | Reset the moving average rate to zero.
clear :: ExponentiallyWeightedMovingAverage -> ExponentiallyWeightedMovingAverage
clear = (initialized .~ False) . (currentRate .~ 0) . (uncounted .~ 0)

-- | Get the current rate of the "ExponentiallyWeightedMovingAverage" for the given window.
rate :: ExponentiallyWeightedMovingAverage -> Double
rate e = (e ^. currentRate) * (e ^. interval)

-- | Create a new "ExpontiallyWeightedMovingAverage" with the given tick interval and averaging window.
empty :: Double -- ^ The interval in seconds between ticks
  -> Minutes -- ^ The duration in minutes which the moving average covers
  -> ExponentiallyWeightedMovingAverage
empty i m = ExponentiallyWeightedMovingAverage 0 0 False i $ makeAlpha i m

-- | Update the moving average based upon the given value
update :: Double -> ExponentiallyWeightedMovingAverage -> ExponentiallyWeightedMovingAverage
update = (uncounted +~)

-- | Update the moving average as if the given interval between ticks has passed.
tick :: ExponentiallyWeightedMovingAverage -> ExponentiallyWeightedMovingAverage
tick e = uncounted .~ 0 $ initialized .~ True $ updateRate e
  where
    instantRate = (e ^. uncounted) / (e ^. interval)
    updateRate a = if a ^. initialized
      then currentRate +~ ((a ^. alpha) * (instantRate - a ^. currentRate)) $ a
      else currentRate .~ instantRate $ a
