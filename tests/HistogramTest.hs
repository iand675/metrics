module HistogramTest where
import Control.Concurrent.Async
import Data.Metrics.Histogram.Internal
import Data.Metrics.Snapshot
import Data.Metrics.Types
import System.Random.MWC
import System.Posix.Time
import Test.QuickCheck

histogramTests :: [Test]
histogramTests =
  [ testUniformSampleMin
  , testUniformSampleMax
  , testUniformSampleMean
  , testUniformSampleMeanThreaded
  , testUniformSample2000
--  , testUniformSample2000Threaded
  , testUniformSampleSnapshot
  , testUniformSampleSnapshotThreaded
  , testExponentialSampleMin
  , testExponentialSampleMax
  , testExponentialSampleMean
  , testExponentialSampleMeanThreaded
  , testExponentialSample2000
  --, testExponentialSample2000Threaded
  , testExponentialSampleSnapshot
  , testExponentialSampleSnapshotThreaded
--  , testExponentialSampleLongIdle
  ]


withUniform :: (Histogram IO -> IO a) -> IO a
withUniform f = do
  seed <- withSystemRandom (asGenIO save)
  h <- uniformHistogram seed
  f h

withExponential :: (Histogram IO -> IO a) -> IO a
withExponential f = do
  seed <- withSystemRandom (asGenIO save)
  t <- epochTime
  h <- exponentiallyDecayingHistogram t seed
  f h

uniformTest :: Assertable a => String -> (Histogram IO -> IO a) -> Test
uniformTest d f = d ~: test $ assert $ withUniform f

exponentialTest :: Assertable a => String -> (Histogram IO -> IO a) -> Test
exponentialTest d f = d ~: test $ assert $ withExponential f

testUniformSampleMin :: Test
testUniformSampleMin = uniformTest "uniform min value" $ \h -> do
  update h 5
  update h 10
  x <- minVal h
  assertEqual "min" 5 x

testUniformSampleMax :: Test
testUniformSampleMax = uniformTest "uniform max value" $ \h -> do
  update h 5
  update h 10
  x <- maxVal h
  assertEqual "max" 10 x

testUniformSampleMean :: Test
testUniformSampleMean = uniformTest "uniform mean value" $ \h -> do
  update h 5
  update h 10
  x <- mean h
  assertEqual "mean" 7.5 x

testUniformSampleMeanThreaded :: Test
testUniformSampleMeanThreaded = uniformTest "async uniform mean value" $ \h -> do
  let task = update h 5 >> update h 10
  asyncs <- sequence $ replicate 10 (async $ task)
  mapM_ wait asyncs
  x <- mean h
  assert $ x == 7.5

testUniformSample2000 :: Test
testUniformSample2000 = uniformTest "uniform sample 2000" $ \h -> do
  mapM_ (update h $) [0..1999]
  x <- maxVal h
  assert $ x == 1999

--testUniformSample2000Threaded :: Test
--testUniformSample2000Threaded ="" ~: test $ do
--	x <- with

testUniformSampleSnapshot :: Test
testUniformSampleSnapshot = uniformTest "uniform snapshot" $ \h -> do
  mapM_ (update h $) [0..99]
  s <- snapshot h
  assert $ median s == 49.5

testUniformSampleSnapshotThreaded :: Test
testUniformSampleSnapshotThreaded = uniformTest "async uniform snapshot" $ \h -> do
  let task = mapM_ (update h $) [0..99]
  asyncs <- sequence $ replicate 10 (async $ task)
  mapM_ wait asyncs
  s <- snapshot h
  assertEqual "median" 49.5 $ median s

testExponentialSampleMin :: Test
testExponentialSampleMin = exponentialTest "minVal" $ \h -> do
  update h 5
  update h 10
  x <- minVal h
  assertEqual "min" 5 x

testExponentialSampleMax :: Test
testExponentialSampleMax = exponentialTest "maxVal" $ \h -> do
  update h 5
  update h 10
  x <- maxVal h
  assertEqual "max" 10 x

testExponentialSampleMean :: Test
testExponentialSampleMean = exponentialTest "mean" $ \h -> do
  update h 5
  update h 10
  x <- mean h
  assertEqual "mean" 7.5 x

testExponentialSampleMeanThreaded :: Test
testExponentialSampleMeanThreaded = exponentialTest "mean threaded" $ \h -> do
  let task = update h 5 >> update h 10
  asyncs <- sequence $ replicate 10 (async $ task)
  mapM_ wait asyncs
  x <- mean h
  assertEqual "mean" 7.5 x

testExponentialSample2000 :: Test
testExponentialSample2000 = exponentialTest "sample 2000" $ \h -> do
  mapM_ (update h $) [0..1999]
  x <- maxVal h
  assertEqual "max" 1999 x

--testExponentialSample2000Threaded :: Test
--testExponentialSample2000Threaded = exponentialTest "async sample 2000" $ \h -> do
--	x <- with

testExponentialSampleSnapshot :: Test
testExponentialSampleSnapshot = exponentialTest "snapshot" $ \h -> do
    mapM_ (update h $) [0..99]
    s <- snapshot h
    assertEqual "median" 49.5 $ median s

testExponentialSampleSnapshotThreaded :: Test
testExponentialSampleSnapshotThreaded = exponentialTest "async snapshot" $ \h -> do
  let task = mapM_ (update h $) [0..99]
  asyncs <- sequence $ replicate 10 (async $ task)
  mapM_ wait asyncs
  s <- snapshot h
  assertEqual "median" 49.5 $ median s

--testExponentialSampleLongIdle :: Test
--testExponentialSampleLongIdle ="" ~: test $ do
--	x <- with
