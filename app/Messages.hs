-- | Module    : Messages
-- Description : Messages what are written to the system log
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : portable
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

-- | For all 'Language' @k@, @k@ is a language which @albertlertd@
-- supports.
data Language = Lojban | English;

-- | @desiredLang@ is the language in which @syslog@ messages are
-- written.
desiredLang :: Language;
desiredLang = Lojban;

-- | @temp k@ describes the temperature of the system whose information
-- is contained within @k@.
temp :: SystemInfo -> String;
temp s = case desiredLang of
  Lojban  -> "le ciste ca kelvo li " ++ t
  English -> "The current system temperature is " ++ t ++ "."
  where t = show $ temperature s;

-- | @temp k@ describes the load average of the system whose information
-- is contained within @k@.
load :: SystemInfo -> String;
load s = case desiredLang of
  Lojban  -> "le mentu samru'e bo cnano ca jibni li " ++ l
  English -> "The one-minute load average is " ++ l ++ "."
  where l = show $ loadAverage1Minute s

-- | @super k@ describes the voltage of the battery of the system whose
-- information is contained within @k@.
super :: SystemInfo -> String;
super s = case desiredLang of
  Lojban  -> "le dicysro ca klanrvolta li " ++ v
  English -> "The current battery voltage is" ++ v ++ "."
  where v = show $ currBatVoltage s;

-- | @currBatVoltage' k@ is the 'Double' value which is contained within
-- @k@.  @currBatVoltage'@ is used instead of @fromJust@ because the
-- quality of @currBatVoltage'@\'s error message is greater than the
-- quality of @fromJust@\'s error message.
currBatVoltage' :: SystemInfo -> Double;
currBatVoltage' = fromMaybe (error blah) . currBatVoltage
  where blah = "Something goes horribly wrong!  currBatVoltage' is \
               \called on a system which lacks a battery, which should \
               \not happen.";
