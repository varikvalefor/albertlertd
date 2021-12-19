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
temp s
  | useLojban = jbTemp
  | otherwise = enTemp
  where
  jbTemp = ".i le ciste ca kelvo li " ++ t
  enTemp = "The current system temperature is " ++ t ++ "."
  t = show $ temperature s;

-- | @temp k@ describes the load average of the system whose information
-- is contained within @k@.
load :: SystemInfo -> String;
load s
  | useLojban = jbLoad
  | otherwise = enLoad
  where
  jbLoad = ".i le mentu bo samru'e cnano ca jibni li " ++ l
  enLoad = "The one-minute load average is " ++ l ++ "."
  l = show $ loadAverage1Minute s

-- | @super k@ describes the voltage of the battery of the system whose
-- information is contained within @k@.
super :: SystemInfo -> String;
super s
  | useLojban = jbSuper
  | otherwise = enSuper
  where
  jbSuper = ".i le dicysro ca klanrvolta li " ++ v
  enSuper = "The current battery voltage is" ++ v ++ "."
  v = show $ currBatVoltage s;

-- | @currBatVoltage' k@ is the 'Double' value which is contained within
-- @k@.  @currBatVoltage'@ is used instead of @fromJust@ because the
-- quality of @currBatVoltage'@\'s error message is greater than the
-- quality of @fromJust@\'s error message.
currBatVoltage' :: SystemInfo -> Double;
currBatVoltage' = fromMaybe (error blah) . currBatVoltage
  where blah = "Something goes horribly wrong!  currBatVoltage' is " ++
               "called on a system which lacks a battery, which " ++
               "should not happen.";
