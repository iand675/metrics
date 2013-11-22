{-# LANGUAGE ExistentialQuantification #-}

-- | A pure moving average module. The interface is agnostic to the scale of time
-- that the average is tracking. It is up to the specific moving average module to
-- handle that functionality.
module Data.Metrics.MovingAverage where

-- | This type encapsulates the interface
-- of the different moving average implementations in such a way that they
-- can be reused without plumbing the types through the other components that
-- use moving averages. Most people won't ever need to use record fields of
-- this type.
data MovingAverage = forall s. MovingAverage
  { movingAverageClear :: !(s -> s)
  -- ^ clear the internal state of the moving average
  , movingAverageUpdate :: !(Double -> s -> s)
  -- ^ add a new sample to the moving average
  , movingAverageTick :: !(s -> s)
  -- ^ perform any modifications of the internal state associated with the passage of a predefined interval of time.
  , movingAverageRate :: !(s -> Double)
  -- ^ get the current rate of the moving average.
  , movingAverageState :: !s
  -- ^ the internal implementation state of the moving average
  }

-- | Reset a moving average back to a starting state.
clear :: MovingAverage -> MovingAverage
clear (MovingAverage c u t r s) = MovingAverage c u t r (c s)

-- | Get the current rate of the moving average.
rate :: MovingAverage -> Double
rate (MovingAverage _ _ _ r s) = r s

-- | Update the average based upon an interval specified by the
-- moving average implementation.
tick :: MovingAverage -> MovingAverage
tick (MovingAverage c u t r s) = MovingAverage c u t r (t s)

-- | Update the average with the specified value.
update :: Double -> MovingAverage -> MovingAverage
update x (MovingAverage c u t r s) = MovingAverage c u t r (u x s)
