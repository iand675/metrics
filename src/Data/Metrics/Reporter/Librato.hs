module Data.Metrics.Reporter.Librato where

import Control.Concurrent
import Control.Monad
import Control.Monad.Trans
import Data.ByteString (ByteString)
import Data.HashMap.Strict (toList)
import Data.Metrics.Internal
import Data.Metrics.Registry (MetricRegistry, metrics)
import qualified Data.Metrics.Registry as R
import Data.Text (Text)
import Data.Metrics.Types
import Data.Vector (fromList)
import Librato.Internal
import Librato.Metrics hiding (metrics)

type Username = ByteString
type Token = ByteString
type Source = Text
type PollInterval = Int

-- TODO: handle exceptions
pollMetrics :: Source -> Username -> Token -> PollInterval -> MetricRegistry IO -> IO ThreadId
pollMetrics s u t i registry = do
	pollThread <- forkIO $ withLibrato u t $ \conf -> do
		forever $ do
			librato conf $ submitAllMetrics s registry
			liftIO $ threadDelay i
	return pollThread

submitAllMetrics :: Source -> MetricRegistry IO -> Librato ()
submitAllMetrics s r = do
	registry <- liftIO $ readMVar $ metrics r
	let ms = toList registry
	mapM_ (uncurry $ submitMetric s) ms

submitMetric :: Source -> Text -> R.Metric IO -> Librato ()
submitMetric s t m = case m of
	R.MetricGauge g -> do
		val <- liftIO $ value g
		submitMetrics (fromList [Measurement t val (Just s) Nothing]) (fromList [])
	R.MetricCounter c -> do
		val <- liftIO $ value c
		submitMetrics (fromList [Measurement t (fromIntegral val) (Just s) Nothing]) (fromList [])
	_ -> return ()