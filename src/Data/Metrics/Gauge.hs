{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
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
import Control.Monad.Primitive
import qualified Data.HashMap.Strict as H
import Data.Metrics.Internal
import Data.Metrics.Types
import Data.Primitive.MutVar

-- | An instantaneous measure of a value.
newtype Gauge m = Gauge { fromGauge :: MV m (m Double) }

-- | Create a new gauge from the given action.
gauge :: PrimMonad m => m Double -> m (Gauge m)
gauge m = do
  r <- newMutVar m
  return $ Gauge r

instance (PrimMonad m) => Value m (Gauge m) Double where
  value (Gauge r) = join $ readMutVar r

instance (PrimMonad m) => Set m (Gauge m) (m Double) where
  set (Gauge r) = updateRef r . const

-- | Compose multiple actions to create a ratio. Useful for graphing percentage information, e. g.
--
-- @
-- connectionUtilizationRate :: IO (Gauge IO)
-- connectionUtilizationRate = gauge $ ratio openConnectionCount $ return connectionPoolSize
-- @
ratio :: Applicative f => f Double -> f Double -> f Double
ratio x y = (/) <$> x <*> y
