module CounterTest where
import Control.Concurrent.Async
import Control.Monad
import Data.Metrics.Counter
import Data.Metrics.Types
import Test.QuickCheck
import Test.QuickCheck.Monadic

counterTests :: [Property]
counterTests =
  [ testIncrement
  , testIncrement'
  , testDecrement
  , testDecrement'
  , testConcurrent
  ]

smallCount :: Gen Int
smallCount = choose (0, 10000)

withCounter :: (Counter IO -> IO a) -> IO a
withCounter f = counter >>= f

testIncrement :: Property
testIncrement = label "single increment" $ monadicIO $ do
  x <- pick smallCount
  x' <- run $ do
    c <- counter
    replicateM_ x $ increment c
    count c
  assert (x == x')

testIncrement' :: Property
testIncrement' = label "higher increment" $ monadicIO $ do
  x <- pick smallCount
  x' <- run $ do
    c <- counter
    increment' c x
    count c
  assert $ x == x'

testDecrement :: Property
testDecrement = label "single decrement" $ monadicIO $ do
  x <- pick smallCount
  x' <- run $ do
    c <- counter
    replicateM_ x $ decrement c
    count c
  assert $ negate x == x'

testDecrement' :: Property
testDecrement' = label "higher decrement" $ monadicIO $ do
  x <- pick smallCount
  x' <- run $ do
    c <- counter
    decrement' c x
    count c
  assert $ negate x == x'

testConcurrent :: Property
testConcurrent = label "concurrently increment" $ monadicIO $ do
  x <- pick smallCount
  y <- pick (choose (0, 40) :: Gen Int)
  r <- run $ do
    c <- counter
    asyncs <- sequence $ take x $ repeat $ async $ replicateM_ y $ increment c
    mapM_ wait asyncs
    count c
  assert $ r == x * y

