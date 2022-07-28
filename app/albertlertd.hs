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
import Data.Bool;
import Data.Maybe;
import SystemInfo;
import System.Exit;
import Control.Monad;
import System.Process;
import qualified Messages as Msg;
import Control.Concurrent (threadDelay);
import System.Posix.Process (forkProcess);

-- | Attention: Men who need documentation of @main@
--
-- "You shouldn't be back here."
main :: IO ();
main = void (forkProcess damn) >> exitSuccess;

-- | @damn@ is the thing which actually serves as the daemon.
damn :: IO ();
damn = nabSystemInfo >>= soundAlarm >> addDelay >> damn
  where
  -- \| A delay is added to ensure that @albertlertd@ does not
  -- demand _too_ much processing power.
  --
  -- If this delay is not present, then @albertlertd@ damn near
  -- constantly runs sysctl(8).  The same results are fetched most of
  -- the time, anyway.
  addDelay = threadDelay (5*10^6);

-- | @soundAlarm k@ sounds some alarms iff @k@ indicates that something
-- goes wrong.
--
-- = Reason for Sounding Individual Alarms
--
-- **Theorem**.  Multiple alarms do not sound simultaneously.
--
-- *Proof*.
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
  | batteryBelowCapacity k = soundTheBatSignal >>
                             soundAlarm k {currBatVoltage = ratedBatVoltage k,
                                           remBatCapacity = (+1) <$> lowBatCapacity k}
                             -- \^ Another infinite loop is prevented.
  | otherwise = return ()
  where
  routine a b = playAudioFile a >> syslog (b k)
  soundOvrLoadAlarm = routine "OVERLOAD.WAV" Msg.load
  soundThermalAlarm = routine "OVERHEAT.WAV" Msg.temp
  soundTheBatSignal = routine "BATSIGNL.WAV" Msg.super;

-- | @batteryBelowCapacity k@ iff the battery of the system which @k@
-- represents is probably almost depleted.
batteryBelowCapacity :: SystemInfo -> Bool;
batteryBelowCapacity k = bool goodMethod jankHack $ all isJust [lC, rC]
  where
  jankHack = maybe False (< 0.9) $ liftM2 (/) cV rV
  -- \^ This hack is used because OpenBSD does not properly read the
  -- remaining capacity of the battery of a terminal which is used by
  -- VARIK.
  --
  -- This hack is potentially excessively cautious... but at least
  -- indicates that the battery is hardly full.
  goodMethod = maybe False id $ liftM2 (<=) rC lC
  goodMethodSupported = all isJust [lC, rC]
  lC = lowBatCapacity k
  rC = remBatCapacity k
  cV = currBatVoltage k
  rV = ratedBatVoltage k;

-- | @isOverloaded k@ iff @k@ indicates that the system is overloaded.
--
-- For all systems, a system is overloaded iff the number of on-line
-- processors which this system contains is less than this system's
-- one-minute load average.
isOverloaded :: SystemInfo -> Bool;
isOverloaded sighs = loadAverage1Minute sighs > numProcessors sighs;

-- | @playAudioFile k@ plays the audio file whose path is
-- /usr/local/share/albertlert/@k@.
playAudioFile :: String
              -- ^ The name of the audio file which is to be played
              -> IO ();
playAudioFile f = void $ readProcess "mplayer" [p] []
  where
  -- \| "@p@" is derived from "path".
  p = "/usr/local/share/albertlert/" ++ f
