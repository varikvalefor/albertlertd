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
import Data.Maybe;
import SystemInfo;
import Control.Monad;
import System.Process;

main :: IO ();
main = nabSystemInfo >>= soundAlarm >> main;

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
  systemIsOverheating
    | isNothing $ currBatVoltage k = False
    | otherwise = fromJust (currBatVoltage k) / fromJust (ratedBatVoltage k) < 0.75
  --
  soundThermalAlarm = playAudioFile "OVERHEAT.WAV"
  soundTheBatSignal = playAudioFile "BATSIGNL.WAV";

-- | @playAudioFile k@ plays the audio file whose name is @k@.
playAudioFile :: String
              -- ^ The name of the audio file which is to be played
              -> IO ();
playAudioFile f = void $ readProcess "mplayer" ["/usr/local/share/albertlert/" ++ f] [];
