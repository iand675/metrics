{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
-- | A module representing a "Gauge", which is simply an action that returns the instantaneous measure of a value for charting.
--
-- The action that provides the gauge's value may be replaced using "set", or read using "value".
--
-- @
-- gaugeExample = do
--   g <- gauge $ return 1
--   x <- value g
--   set g $ return 2
--   y <- value g
--   return (x == 1 && y == 2)
-- @
module Data.Metrics.Gauge (
  Gauge,
  gauge,
  ratio,
  module Data.Metrics.Types
) where
import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Primitive
import Data.Metrics.Internal
import Data.Metrics.Types
import Data.Primitive.MutVar

-- | An instantaneous measure of a value.
newtype Gauge m = Gauge { fromGauge :: MV m (m Double) }

-- | Create a new gauge from the given action.
gauge :: (MonadBase b m, PrimMonad b) => b Double -> m (Gauge b)
gauge m = do
  r <- liftBase $ newMutVar m
  return $ Gauge r

instance (MonadBase b m, PrimMonad b) => Value b m (Gauge b) Double where
  value (Gauge r) = liftBase $ join $ readMutVar r
  {-# INLINEABLE value #-}

instance (MonadBase b m, PrimMonad b) => Set b m (Gauge b) (b Double) where
  set (Gauge r) = liftBase . updateRef r . const
  {-# INLINEABLE set #-}

-- | Compose multiple actions to create a ratio. Useful for graphing percentage information, e. g.
--
-- @
-- connectionUtilizationRate :: IO (Gauge IO)
-- connectionUtilizationRate = gauge $ ratio openConnectionCount $ return connectionPoolSize
-- @
ratio :: Applicative f => f Double -> f Double -> f Double
ratio x y = (/) <$> x <*> y

