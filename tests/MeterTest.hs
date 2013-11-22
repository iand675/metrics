{-# LANGUAGE Rank2Types #-}
module MeterTest where
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Metrics.Internal
import Data.Metrics.Meter.Internal
import Data.Metrics.Types
import Data.Primitive.MutVar
import Data.STRef
import Test.QuickCheck
import Test.QuickCheck.Monadic
import System.Posix.Types

smallCount :: Gen Int
smallCount = choose (0, 10000)

increment1s :: forall s. STRef s EpochTime -> ST s EpochTime
increment1s r = do
  modifySTRef r succ
  t <- readSTRef r
  return t

run1sMeter :: (forall s. Meter (ST s) -> ST s a) -> a
run1sMeter f = runST $ do
  r <- newSTRef 0
  m <- mkMeter $ increment1s r
  f m

meterCountTest :: Property
meterCountTest = label "mark increments count" $ monadicST $ do
  x <- pick smallCount
  let c = run1sMeter $ \m -> (replicateM_ x $ mark m) >> count m
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
  return $ (meterLastTick md, x)
