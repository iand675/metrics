{-# LANGUAGE Rank2Types #-}
module MeterTest where
import Control.Lens
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Metrics.Internal
import Data.Metrics.Meter
import Data.Metrics.Meter.Internal (lastTick)
import Data.Metrics.Types
import Data.Primitive.MutVar
import Data.STRef
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Posix.Types


smallCount :: Gen Int
smallCount = choose (0, 10000)


increment1s :: Num a => STRef s a -> ST s a
increment1s r = do
  modifySTRef r (+ 1)
  readSTRef r


run1sMeter :: (forall s. Meter (ST s) -> ST s a) -> a
run1sMeter f = runST $ do
  r <- newSTRef 0
  m <- mkMeter $ increment1s r
  f m


meterCountTest :: Property
meterCountTest = label "mark increments count" $ monadicST $ do
  x <- pick smallCount
  let c = run1sMeter $ \m -> do
        replicateM_ x (mark m)
        count m
  assert $ x == c
-- testMeter
-- testMeterThreaded
-- testOneMinuteRate


testTicks = runST $ do
  r <- newSTRef 0
  m <- mkMeter $ increment1s r
  mark m
  mark m
  mark m
  mark m
  mark m
  md <- readMutVar (fromMeter m)
  x <- readSTRef r
  return $ (md ^. lastTick, x)


