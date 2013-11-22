{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-- |
-- Module      : Data.Metrics.Counter
-- Copyright   : (c) Ian Duncan 2013
-- Stability   : experimental
-- Portability : non-portable
--
-- An incrementing and decrementing counter metric
--
-- > import Data.Metrics.Counter
-- >
-- > main :: IO ()
-- > main = do
-- >   c <- counter
-- >   increment c
-- >   x <- value c
-- >   print $ x == 1
--
module Data.Metrics.Counter (
  Counter,
  counter,
  increment,
  increment',
  decrement,
  decrement',
  module Data.Metrics.Types
) where
import Control.Monad.Primitive
import qualified Data.HashMap.Strict as H
import Data.Metrics.Internal
import Data.Metrics.Types
import Data.Primitive.MutVar

-- | A basic atomic counter.
newtype Counter m = Counter { fromCounter :: MV m Int }

instance PrimMonad m => Count m (Counter m) where
  count (Counter ref) = readMutVar ref

instance PrimMonad m => Value m (Counter m) Int where
  value (Counter ref) = readMutVar ref

instance PrimMonad m => Set m (Counter m) Int where
  set (Counter ref) x = updateRef ref (const x)

instance PrimMonad m => Clear m (Counter m) where
  clear c = set c 0

-- | Create a new counter.
counter :: (Functor m, PrimMonad m) => m (Counter m)
counter = fmap Counter $ newMutVar 0

-- | Bump up a counter by 1.
increment :: PrimMonad m => Counter m -> m ()
increment = flip increment' 1

-- | Add an arbitrary amount to a counter.
increment' :: PrimMonad m => Counter m -> Int -> m ()
increment' (Counter ref) x = updateRef ref (+ x)

-- | Decrease the value of a counter by 1.
decrement :: PrimMonad m => Counter m -> m ()
decrement = flip decrement' 1

-- | Subtract an arbitrary amount from a counter.
decrement' :: PrimMonad m => Counter m -> Int -> m ()
decrement' (Counter ref) x = updateRef ref (subtract x)
