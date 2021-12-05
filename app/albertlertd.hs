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
soundAlarm k
  | temperature k > 350 = soundThermalAlarm >>
                          soundAlarm k {temperature = 0}
  | systemIsOverheating = soundTheBatSignal >>
                          soundAlarm k {currBatVoltage = ratedBatVoltage k}
  | otherwise = return ()
  where
  systemIsOverheating :: Bool
  systemIsOverheating = currBatVoltage k / ratedBatVoltage k < 0.75
  --
  soundThermalAlarm = playAudioFile "OVERHEAT.WAV"
  soundTheBatSignal = playAudioFile "BATSIGNL.WAV";

-- | @playAudioFile k@ plays the audio file whose name is @k@.
playAudioFile :: String
              -- ^ The name of the audio file which is to be played
              -> IO ();
playAudioFile = error "This thing is unimplemented.";
