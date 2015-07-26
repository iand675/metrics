{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE KindSignatures         #-}
-- | The main accessors for common stateful metric implementation data.
module Data.Metrics.Types where
import Control.Concurrent.MVar
import Control.Monad.Primitive
import Data.Functor.Identity
import Data.HashMap.Strict (HashMap)
import Data.Metrics.Internal
import Data.Metrics.Snapshot
import Data.Primitive.MutVar
import Data.Text (Text)
import Data.Vector.Unboxed (Vector)

-- | Histogram moving averages are tracked (by default) on minute scale.
type Minutes = Int

-- | Get the current count for the given metric.
class Count (b :: * -> *) m a | m -> b, a -> b where
  -- | retrieve a count
  count :: a -> m Int

-- | Provides statistics from a histogram that tracks the standard moving average rates.
class Rate (b :: * -> *) m a | m -> b, a -> b where
  -- | Get the average rate of occurrence for some sort of event for the past minute.
  oneMinuteRate :: a -> m Double
  -- | Get the average rate of occurrence for some sort of event for the past five minutes.
  fiveMinuteRate :: a -> m Double
  -- | Get the average rate of occurrence for some sort of event for the past fifteen minutes.
  fifteenMinuteRate :: a -> m Double
  -- | Get the mean rate of occurrence for some sort of event for the entirety of the time that 'a' has existed.
  meanRate :: a -> m Double

-- | Gets the current value from a simple metric (i.e. a "Counter" or a "Gauge")
class Value (b :: * -> *) m a v | m -> b, a -> b v where
  value :: a -> m v

-- | Update a metric by performing wholesale replacement of a value.
class Set (b :: * -> *) m a v | m -> b, a -> b v where
  -- | Replace the current value of a simple metric (i.e. a "Counter" or a "Gauge")
  set :: a -> v -> m ()

-- | Provides a way to reset metrics. This might be useful in a development environment
-- or to periodically get a clean state for long-running processes.
class Clear (b :: * -> *) m a | m -> b, a -> b where
  -- | Reset the metric to an 'empty' state. In practice, this should be
  -- equivalent to creating a new metric of the same type in-place.
  clear :: a -> m ()

-- | Provides the main interface for retrieving statistics tabulated by a histogram.
class Statistics (b :: * -> *) m a | m -> b, a -> b where
  -- | Gets the highest value encountered thus far.
  maxVal :: a -> m Double
  -- | Gets the lowest value encountered thus far.
  minVal :: a -> m Double
  -- | Gets the current average value. This may have slightly different meanings
  -- depending on the type of "MovingAverage" used.
  mean :: a -> m Double
  -- | Gets the standard deviation of all values encountered this var.
  stddev :: a -> m Double
  -- | Gets the variance of all values encountered this var.
  variance :: a -> m Double

-- | Update statistics tracked by a metric with a new sample.
class Update (b :: * -> *) m a v | m -> b, a -> b v where
  -- | Feed a metric another value.
  update :: a -> v -> m ()

-- | Take a snapshot (a sorted vector) of samples used for calculating quantile data.
class TakeSnapshot (b :: * -> *) m a | m -> b, a -> b where
  -- | Get a sample of the values currently in a histogram or type that contains a histogram.
  snapshot :: a -> m Snapshot

