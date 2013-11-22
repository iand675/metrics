module Main where
import Control.Monad
import CounterTest
import GaugeTest
import EDR
import EWMA
-- import HistogramTest
import Test.QuickCheck
--import MeterTest
--import RegistryTest
--import TimerTest

main = mapM_ quickCheck $
  counterTests ++
  gaugeTests ++
  ewmaTests ++
  edrTests
