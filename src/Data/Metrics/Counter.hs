{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}
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
import Control.Monad.Base
import Control.Monad.Primitive
import qualified Data.HashMap.Strict as H
import Data.Metrics.Internal
import Data.Metrics.Types
import Data.Primitive.MutVar

-- | A basic atomic counter.
newtype Counter m = Counter { fromCounter :: MV m Int }

instance (MonadBase b m, PrimMonad b) => Count b m (Counter b) where
  count (Counter ref) = liftBase $ readMutVar ref
  {-# INLINEABLE count #-}

instance (MonadBase b m, PrimMonad b) => Value b m (Counter b) Int where
  value (Counter ref) = liftBase $ readMutVar ref
  {-# INLINEABLE value #-}

instance (MonadBase b m, PrimMonad b) => Set b m (Counter b) Int where
  set (Counter ref) x = liftBase $ updateRef ref (const x)
  {-# INLINEABLE set #-}

instance (MonadBase b m, PrimMonad b) => Clear b m (Counter b) where
  clear c = set c 0
  {-# INLINEABLE clear #-}

-- | Create a new counter.
counter :: (MonadBase b m, PrimMonad b) => m (Counter b)
counter = liftBase $ fmap Counter $ newMutVar 0
{-# INLINEABLE counter #-}

-- | Bump up a counter by 1.
increment :: PrimMonad m => Counter m -> m ()
increment = flip increment' 1
{-# INLINEABLE increment #-}

-- | Add an arbitrary amount to a counter.
increment' :: PrimMonad m => Counter m -> Int -> m ()
increment' (Counter ref) x = updateRef ref (+ x)
{-# INLINEABLE increment' #-}

-- | Decrease the value of a counter by 1.
decrement :: PrimMonad m => Counter m -> m ()
decrement = flip decrement' 1
{-# INLINEABLE decrement #-}

-- | Subtract an arbitrary amount from a counter.
decrement' :: PrimMonad m => Counter m -> Int -> m ()
decrement' (Counter ref) x = updateRef ref (subtract x)
{-# INLINEABLE decrement' #-}

