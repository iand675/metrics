-- | Internal helpers that provide strict atomic MutVar access.
--
-- These functions allow us to avoid the overhead of MVar as long
-- as we can factor the impure sections of code out in such a way
-- that the pure metric calculations can be executed without requiring
-- access to multiple MutVars at a time.
module Data.Metrics.Internal (
  updateRef,
  applyWithRef,
  updateAndApplyToRef,
  MV
) where
import Control.Monad.Primitive
import Data.Primitive.MutVar

-- | Perform a strict update on a MutVar. Pretty much identical to the strict variant of atomicModifyIORef.
updateRef :: PrimMonad m => MV m a -> (a -> a) -> m ()
updateRef r f = do
  b <- atomicModifyMutVar r (\x -> let (a, b) = (f x, ()) in (a, a `seq` b))
  b `seq` return b

-- | Strictly apply a function on a MutVar while blocking other access to it.
--
-- I really think this is probably not implemented correctly in terms of being excessively strict.
applyWithRef :: PrimMonad m => MV m a -> (a -> b) -> m b
applyWithRef r f = do
  b <- atomicModifyMutVar r (\x -> let app = f x in let (a, b) = (x, app) in (a, a `seq` b))
  b `seq` return b

-- | A function which combines the previous two, updating a value atomically
-- and then returning some value calculated with the update in a single step.
updateAndApplyToRef :: PrimMonad m => MV m a -> (a -> a) -> (a -> b) -> m b
updateAndApplyToRef r fa fb = do
  b <- atomicModifyMutVar r $ \x ->
    let appA = fa x in
    let appB = fb appA in
    let (a, b) = (appA, appB) in
    (a, a `seq` b)
  b `seq` return b

-- | MutVar (PrimState m) is a little verbose.
type MV m = MutVar (PrimState m)
