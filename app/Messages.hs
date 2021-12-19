-- | Module    : Messages
-- Description : Messages what are written to the system log
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains some functions which generate messages which are
-- to be written to the system log.
module Messages (
  temp,
  load,
  super
) where
import SystemInfo;
import Data.Maybe;

-- | @useLojban@ iff messages should be written to the system log in
-- Lojban.
useLojban :: Bool;
useLojban = True;

-- | @temp k@ describes the temperature of the system whose information
-- is contained within @k@.
temp :: SystemInfo -> String;
temp
  | useLojban = jbTemp
  | otherwise = enTemp
  where
  jbTemp s = ".i le ciste ca kelvo li " ++ show (temperature s)
  enTemp s = "The current system temperature is " ++
           show (temperature s) ++ ".";

-- | @temp k@ describes the load average of the system whose information
-- is contained within @k@.
load :: SystemInfo -> String;
load
  | useLojban = jbLoad
  | otherwise = enLoad
  where
  jbLoad s = ".i le mentu bo samru'e cnano ca jibni li " ++
             show (loadAverage1Minute s) ++ ".";
  enLoad s = "The one-minute load average is " ++
             show (loadAverage1Minute s) ++ ".";

-- | @super k@ describes the voltage of the battery of the system whose
-- information is contained within @k@.
super :: SystemInfo -> String;
super
  | useLojban = jbSuper
  | otherwise = enSuper
  where
  jbSuper s = ".i le dicysro ca klanrvolta li " ++
              show (currBatVoltage' s);
  enSuper s = "The current battery voltage is" ++
              show (currBatVoltage' s) ++ ".";

-- | @currBatVoltage' k@ is the 'Double' value which is contained within
-- @k@.  @currBatVoltage'@ is used instead of @fromJust@ because the
-- quality of @currBatVoltage'@\'s error message is greater than the
-- quality of @fromJust@\'s error message.
currBatVoltage' :: SystemInfo -> Double;
currBatVoltage' = fromMaybe (error blah) . currBatVoltage
  where blah = "Something goes horribly wrong!  currBatVoltage' is " ++
               "called on a system, which should not happen.";
