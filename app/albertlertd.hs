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
import Syslog;
import Data.Maybe;
import SystemInfo;
import System.Exit;
import Control.Monad;
import System.Process;
import Control.Concurrent (threadDelay);
import System.Posix.Process (forkProcess);

-- | Attention: Men who need documentation of @main@
--
-- "You shouldn't be back here."
main :: IO ();
main = void (forkProcess damn) >> exitSuccess;

-- | @damn@ is the function which actually serves as the daemon.
damn :: IO ();
damn = nabSystemInfo >>= soundAlarm >> threadDelay (5*10^6) >> damn;
-- A delay is added to ensure that @albertlertd@ does not
-- demand _too_ much processing power.
--
-- If this delay is not present, then @albertlertd@ damn near
-- constantly runs sysctl(8).  The same results are fetched most of
-- the time, anyway.

-- | @soundAlarm k@ sounds some alarms iff @k@ indicates that something
-- goes wrong.
--
--
--
-- **Theorem**.  Multiple alarms do not sound simultaneously.
--
-- *Proof.
--
-- Multiple alarms sound simultaneously only if the ease of
-- understanding the output of @albertlertd@ is not maximised.
--
-- The ease of understanding the output of @albertlertd@ is maximised.
--
-- Therefore, multiple alarms do not sound simultaneously.  Q.E.D.
soundAlarm :: SystemInfo -> IO ();
soundAlarm k
  | temperature k > 350 = soundThermalAlarm >>
                          soundAlarm k {temperature = 0}
                          -- \^ By setting @k@'s temperature to 0 before
                          -- the recursion takes place, @soundAlarm@
                          -- prevents an infinite loop.
  | batteryIsUnderVolted k = soundTheBatSignal >>
                             soundAlarm k {currBatVoltage = ratedBatVoltage k}
                             -- \^ Another infinite loop is prevented.
  | otherwise = return ()
  where
  soundThermalAlarm = playAudioFile "OVERHEAT.WAV" >>
                      syslog ("The current system temperature is " ++
                              show (temperature k) ++ ".")
  soundTheBatSignal = playAudioFile "BATSIGNL.WAV" >>
                      syslog("The current battery voltage is " ++
                             show (currBatVoltage k) ++ " kelvins.");

-- | @batteryIsUnderVolted k@ iff the battery of the system which @k@
-- represents is probably almost depleted.
batteryIsUnderVolted :: SystemInfo -> Bool;
batteryIsUnderVolted k
  | isNothing $ currBatVoltage k = False
  | otherwise = cV / rV < 0.9
  -- \^ This hack is used because OpenBSD does not properly read the
  -- remaining capacity of VARIK's primary terminal.
  --
  -- This hack is potentially excessively cautious... but at least
  -- indicates that the battery is hardly full.
  where
  cV = fromJust (currBatVoltage k)
  rV = fromJust (ratedBatVoltage k);

-- | @playAudioFile k@ plays the audio file whose path is
-- /usr/local/share/albertlert/@k@.
playAudioFile :: String
              -- ^ The name of the audio file which is to be played
              -> IO ();
playAudioFile f = void $ readProcess "mplayer" ["/usr/local/share/albertlert/" ++ f] [];
