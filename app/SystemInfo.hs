{-# LANGUAGE CPP #-}

-- | Module    : SystemInfo
-- Description : System-information-grabbing crap
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contains 'SystemInfo' and @'nabSystemInfo'@.
--
-- WARNING: This module makes heavy use of ifdefs.  Luckily, the use of
-- such ifdefs is at least somewhat justifiable.  However, the reader
-- _must_ pay attention to the boundaries of such ifdefs.
module SystemInfo (
  SystemInfo(..),
  nabSystemInfo
) where
import Data.Maybe;
import System.Exit;
import System.Process;
import Data.List.Split (splitOn);

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
  currBatVoltage :: Maybe Double,
  -- | If @l@ has a battery, then @currBatVoltage k@ is 'Just' the
  -- rated voltage of the primary battery of @l@.
  --
  -- If @k@ lacks a battery, then @currBatVoltage k@ is 'Nothing'.
  ratedBatVoltage :: Maybe Double
} deriving (Show);

-- | @nabSystemInfo@ returns 'SystemInfo' regarding the system on which
-- @nabSystemInfo@ is run.
nabSystemInfo :: IO SystemInfo;
#ifdef openbsd_HOST_OS
nabSystemInfo =
  getInfo >>= \info ->
  return SystemInfo {
    temperature = fromJust (info !! 0) + 273.15,
    -- \^ C-to-K conversion occurs here.
    ratedBatVoltage = info !! 1,
    currBatVoltage = info !! 2
  };

-- | @getInfo@ returns the list of the Celsius-based temperature of the
-- system, the rated voltage of the system's primary battery, and the
-- current voltage of the system's battery.
--
-- Why in the hell is this comment a piece of Haddock documentation?
-- Haddock is blissfully* unaware of the existence of @getInfo@.
--
-- *Haddock is _probably_ incapable of experiencing bliss.
getInfo :: IO [Maybe Double];
getInfo = map extractDoubleValue <$> mapM getValue sysctlNames
  where
  -- \| Documenting the contents of @sysctlNames@ is briefly considered.
  -- However, VARIK finds that such documentation is probably
  -- unnecessary; @sysctlNames@ should be pretty self-explanatory.
  sysctlNames = ["hw.sensors.cpu0.temp0",
                 "hw.sensors.acpibat0.volt0",
                 "hw.sensors.acpibat0.volt1"]
  getValue a = readProcessWithExitCode "sysctl" [a] [];

-- | Where @(a,b,c)@ is the output of a sysctl(8) command which is run
-- via @'readProcessWithExitCode'@, if @b@ contains a 'Double', then
-- @extractDoubleValue k@ 'Just' returns this 'Double'.
-- @extractDoubleValue k@ otherwise outputs 'Nothing'.
extractDoubleValue :: (ExitCode, String, String) -> Maybe Double;
extractDoubleValue (exitcode, stdout, stderr)
  | exitcode == ExitSuccess = Just $
                              -- \^ Because a value actually exists, a
    {-     @       @     -}   -- value can be safely returned.
    {-      @     @      -}   read $
    {-       @   @       -}   -- \| Whatever unit which follows the
    {-        @ @        -}   -- space can be safely discarded; other
    {-         @         -}   -- parts of this program account for
    {- @               @ -}   -- such units.
    {-  @@           @@  -}   head $ splitOn " " $
    {-    @@@@@@@@@@@    -}   -- \| Take the thing which FOLLOWS the
                              -- equals sign, dumb-ass.
                              (!!1) $ splitOn "=" stdout
  | otherwise = Nothing;
#else
nabSystemInfo = error $ "nabSystemInfo is unfamiliar with " ++
                        "this operating system.";
#endif
