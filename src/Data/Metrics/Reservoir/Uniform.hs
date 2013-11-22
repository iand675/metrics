-- | A histogram with a uniform reservoir produces quantiles which are valid for the entirely of the histogram’s lifetime.
-- It will return a median value, for example, which is the median of all the values the histogram has ever been updated with.
-- It does this by using an algorithm called Vitter’s R), which randomly selects values for the reservoir with linearly-decreasing probability.
--
-- Use a uniform histogram when you’re interested in long-term measurements.
-- Don’t use one where you’d want to know if the distribution of the underlying data stream has changed recently.
module Data.Metrics.Reservoir.Uniform (
  UniformReservoir,
  reservoir,
  unsafeReservoir,
  clear,
  unsafeClear,
  size,
  snapshot,
  update,
  unsafeUpdate
) where
import Control.Monad.ST
import Data.Metrics.Internal
import Data.Time.Clock
import qualified Data.Metrics.Reservoir as R
import qualified Data.Metrics.Snapshot as S
import Data.Primitive.MutVar
import System.Random.MWC
import qualified Data.Vector.Unboxed as I
import qualified Data.Vector.Unboxed.Mutable as V

-- | Make a safe uniform reservoir. This variant provides safe access at the expense of updates costing O(n)
reservoir :: Seed
  -> Int -- ^ maximum reservoir size
  -> R.Reservoir
reservoir g r = R.Reservoir
  { R._reservoirClear = clear
  , R._reservoirSize = size
  , R._reservoirSnapshot = snapshot
  , R._reservoirUpdate = update
  , R._reservoirState = UniformReservoir 0 (I.replicate r 0) g
  }

-- | Using this variant requires that you ensure that there is no sharing of the reservoir itself.
--
-- In other words, there must only be a single point of access (an IORef, etc. that accepts some sort of modification function).
--
-- In return, updating the reservoir becomes an O(1) operation and clearing the reservoir avoids extra allocations.
unsafeReservoir :: Seed -> Int -> R.Reservoir
unsafeReservoir g r = R.Reservoir
  { R._reservoirClear = unsafeClear
  , R._reservoirSize = size
  , R._reservoirSnapshot = snapshot
  , R._reservoirUpdate = unsafeUpdate
  , R._reservoirState = UniformReservoir 0 (I.replicate r 0) g
  }

-- | A reservoir in which all samples are equally likely to be evicted when the reservoir is at full capacity.
--
-- This is conceptually simpler than the "ExponentiallyDecayingReservoir", but at the expense of providing a less accurate sample.
data UniformReservoir = UniformReservoir
  { _urCount :: !Int
  , _urReservoir :: !(I.Vector Double)
  , _urSeed :: !Seed
  }

-- | Reset the reservoir to empty.
clear :: NominalDiffTime -> UniformReservoir -> UniformReservoir
clear = go
  where
    go _ c = c { _urCount = 0, _urReservoir = newRes $ _urReservoir c }
    newRes v = runST $ do
      v' <- I.thaw v
      V.set v' 0
      I.unsafeFreeze v'

-- | Reset the reservoir to empty by performing an in-place modification of the reservoir.
unsafeClear :: NominalDiffTime -> UniformReservoir -> UniformReservoir
unsafeClear = go
  where
    go _ c = c { _urCount = 0, _urReservoir = newRes $ _urReservoir c }
    newRes v = runST $ do
      v' <- I.unsafeThaw v
      V.set v' 0
      I.unsafeFreeze v'

-- | Get the current size of the reservoir
size :: UniformReservoir -> Int
size = go
  where
    go c = min (_urCount c) (I.length $ _urReservoir c)

-- | Take a snapshot of the reservoir by doing an in-place unfreeze.
--
-- This should be safe as long as unsafe operations are performed appropriately.
snapshot :: UniformReservoir -> S.Snapshot
snapshot = go
  where
    go c = runST $ do
      v' <- I.unsafeThaw $ _urReservoir c
      S.takeSnapshot $ V.slice 0 (size c) v'

-- | Perform an update of the reservoir by copying the internal vector. O(n)
update :: Double -> NominalDiffTime -> UniformReservoir -> UniformReservoir
update = go
  where
    go x _ c = c { _urCount = newCount, _urReservoir = newRes, _urSeed = newSeed }
      where
        newCount = succ $ _urCount c
        (newSeed, newRes) = runST $ do
          v' <- I.thaw $ _urReservoir c
          g <- restore (_urSeed c)
          if newCount <= V.length v'
            then V.unsafeWrite v' (_urCount c) x
            else do
              i <- uniformR (0, newCount) g
              if i < V.length v'
                then V.unsafeWrite v' i x
                else return ()
          v'' <- I.unsafeFreeze v'
          s <- save g
          return (s, v'')

-- | Perform an in-place update of the reservoir. O(1)
unsafeUpdate :: Double -> NominalDiffTime -> UniformReservoir -> UniformReservoir
unsafeUpdate = go
  where
    go x _ c = c { _urCount = newCount, _urReservoir = newRes, _urSeed = newSeed }
      where
        newCount = succ $ _urCount c
        (newSeed, newRes) = runST $ do
          v' <- I.unsafeThaw $ _urReservoir c
          g <- restore (_urSeed c)
          if newCount <= V.length v'
            then V.unsafeWrite v' (_urCount c) x
            else do
              i <- uniformR (0, newCount) g
              if i < V.length v'
                then V.unsafeWrite v' i x
                else return ()
          v'' <- I.unsafeFreeze v'
          s <- save g
          return (s, v'')
