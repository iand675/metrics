{-# LANGUAGE ExistentialQuantification #-}
-- | A reservoir is the internal storage mechanism for a "Histogram".
-- It provides a generic way to store histogram values in a way that
-- allows us to avoid the need to plumb the implementation type through anything
-- that uses a reservoir.
module Data.Metrics.Reservoir where
import Data.Metrics.Snapshot
import Data.Time.Clock

-- | Encapsulates the internal state of a reservoir implementation.
--
-- The two standard implementations are the ExponentiallyDecayingReservoir and the UniformReservoir.
data Reservoir = forall s. Reservoir
  { reservoirClear :: !(NominalDiffTime -> s -> s)
  -- ^ An operation that resets a reservoir to its initial state
  , reservoirSize :: !(s -> Int)
  -- ^ Retrieve current size of the reservoir.
  -- This may or may not be constant depending on the specific implementation.
  , reservoirSnapshot :: !(s -> Snapshot)
  -- ^ Take snapshot of the current reservoir.
  --
  -- The number of items in the snapshot should always match the reservoir's size.
  , reservoirUpdate :: !(Double -> NominalDiffTime -> s -> s)
  -- ^ Add a new value to the reservoir, potentially evicting old values in the prcoess.
  , reservoirState :: !s
  -- ^ The internal state of the reservoir.
  }

-- | Reset a reservoir to its initial state.
clear :: NominalDiffTime -> Reservoir -> Reservoir
clear t (Reservoir c size ss u st) = Reservoir c size ss u (c t st)

-- | Get the current number of elements in the reservoir
size :: Reservoir -> Int
size (Reservoir _ size _ _ st) = size st

-- | Get a copy of all elements in the reservoir.
snapshot :: Reservoir -> Snapshot
snapshot (Reservoir _ _ ss _ st) = ss st

-- | Update a reservoir with a new value.
--
-- N.B. for some reservoir types, the latest value is not guaranteed to be retained in the reservoir.
update :: Double -> NominalDiffTime -> Reservoir -> Reservoir
update x t (Reservoir c size ss u st) = Reservoir c size ss u (u x t st)
