-- |
-- Module      : Data.HealthCheck
-- Copyright   : (c) Ian Duncan 2013
-- Stability   : experimental
-- Portability : non-portable
--
-- A simple interface through which simple status dashboards can be built.
--
-- > import Data.HealthCheck
-- > import Data.Metrics.Reporter.StdOut
-- > 
-- > healthCheck1 :: HealthCheck
-- > healthCheck1 = healthCheck "benign_warm_fuzzy_thing" $
-- >   return $ StatusReport Good Nothing
-- > 
-- > healthCheck2 :: HealthCheck
-- > healthCheck2 = healthCheck "nuclear_missile_launcher" $
-- >   return $ StatusReport Ugly $ Just "out of missiles"
-- > 
-- > main :: IO ()
-- > main = printHealthChecks [ healthCheck1, healthCheck2 ]
--
module Data.HealthCheck (
  HealthCheck(..),
  HealthChecks,
  healthCheck,
  Status(..),
  StatusReport(..)
) where
import Data.Text (Text)

-- | Clean up type signatures for bundling sets of health checks for reporting
type HealthChecks = [HealthCheck]

-- | A simple discrete health reporter
data HealthCheck = HealthCheck
  { healthCheckStatusReport :: IO StatusReport -- ^ An action which determines the current status of the health check
  , healthCheckName :: Text -- ^ A unique identifier for the health check
  }

-- | Provides a simple status reporting mechanism for checking application health at a glance.
data Status
  = Good -- ^ Everything appears to be going well.
  | Bad -- ^ Something is broken.
  | Ugly -- ^ There is some sort of non-critical issue that deserves attention.
  | Unknown 
    -- ^ There is no information, either good or bad, at the moment.
    -- An example of this might be something like a loss of network connectivity to a non-crucial service.
  deriving (Read, Show, Eq, Ord)

-- | A report on the current status of a subsystem.
data StatusReport = StatusReport
  { status :: Status -- ^ Current status
  , statusMessage :: Maybe Text -- ^ An optional message to display about the current status.
  } deriving (Show)

-- | Create a health check.
healthCheck :: Text -> IO StatusReport -> HealthCheck
healthCheck = flip HealthCheck