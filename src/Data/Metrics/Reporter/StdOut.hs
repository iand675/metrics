{-# LANGUAGE OverloadedStrings #-}
-- | Logging to stdout is primarily intended for development purposes or creating command line status tools.
--
-- For more meaningful access to statistics, metrics should be sent to something like Librato or Graphite.
module Data.Metrics.Reporter.StdOut (
  printHealthCheck,
  printHealthChecks
) where
import qualified Data.HashMap.Strict as H
import Data.HealthCheck
import Data.Metrics.Internal
import Data.Metrics.Types
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.ANSI

--prettyPrintMetric (m, v) = T.putStr m >> T.putStr ": " >> putStrLn (show v)

--reportMetrics :: MetricRegistry -> IO ()
--reportMetrics m = dumpMetrics m >>= mapM_ prettyPrintMetric

--dumpMetrics :: MetricRegistry -> IO [(T.Text, Double)]
--dumpMetrics r = do
--  ms <- readMVar $ metrics r
--  -- let readRep (k, (repAction, _)) = repAction >>= \rep -> return (k, rep)
--  -- mapM readRep $ H.toList ms
--  return []
fg = SetColor Foreground Vivid

-- | Pretty-print a single HealthCheck to the console using ANSI colors.
printHealthCheck :: HealthCheck -> IO ()
printHealthCheck (HealthCheck m name) = do
  s <- m
  setSGR $ case status s of
    Good -> [fg Green]
    Bad -> [fg Red]
    Ugly -> [fg Yellow]
    Unknown -> [fg Cyan]
  T.putStr "● "
  setSGR [Reset]
  T.putStr name
  maybe (T.putStr "\n") (\msg -> T.putStr ": " >> T.putStrLn msg) $ statusMessage s

-- | Pretty-print a list of HealthChecks to the console using ANSI colors.
printHealthChecks :: HealthChecks -> IO ()
printHealthChecks = mapM_ printHealthCheck