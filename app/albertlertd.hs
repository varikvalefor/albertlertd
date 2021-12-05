-- | Module    : Main
-- Description : Most high-level part of albertlertd(8)
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains the most high-level bits of albertlertd(8)'s
-- source code.
module Main where

-- | SystemInfo holds information regarding some arbitrary computer
-- system.
--
-- Within the documentation of this thing, @l@ refers to the system
-- whose information is contained within 'SystemInfo' value @k@.
data SystemInfo = SystemInfo {
  -- | @temperature k@ is the Kelvin-based temperature of @l@.
  temperature :: Double,
  -- | If @l@ has a battery, then @currBatVoltage k@ is 'Just' the
  -- current voltage of the primary battery of @l@.
  --
  -- If @l@ lacks a battery, then @currBatVoltage k@ is 'Nothing'.
  currBatVoltage :: Double,
  -- | If @l@ has a battery, then @currBatVoltage k@ is 'Just' the
  -- rated voltage of the primary battery of @l@.
  --
  -- If @k@ lacks a battery, then @currBatVoltage k@ is 'Nothing'.
  ratedBatVoltage :: Double
} deriving (Show);

main :: IO ();
main = nabSystemInfo >>= soundAlarm >> main;

-- | @nabSystemInfo@ returns 'SystemInfo' regarding the system on which
-- @nabSystemInfo@ is run.
nabSystemInfo :: IO SystemInfo;
nabSystemInfo = error "This thing is unimplemented.";

-- | @soundAlarm k@ sounds some alarms iff @k@ indicates that something
-- goes wrong.
soundAlarm :: SystemInfo -> IO ();
soundAlarm = error "This thing is unimplemented.";
