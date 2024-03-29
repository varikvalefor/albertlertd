{-# LANGUAGE CPP #-}

-- | Module    : SystemInfo
-- Description : System-information-grabbing crap
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : OpenBSD
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
import Text.Read;
import Data.Maybe;
import System.Exit;
import System.Process;
import Data.List.Split (splitOn);

-- | SystemInfo records hold information regarding arbitrary computer
-- systems.
--
-- Within the documentation of this thing, @l@ refers to the system
-- whose information is contained within 'SystemInfo' value @k@.
data SystemInfo = SystemInfo {
  -- | @temperature k@ is the Kelvin-based temperature of @l@.
  temperature :: !Double,
  -- | If @l@ has a battery, then @currBatVoltage k@ is 'Just' the
  -- current voltage of the primary battery of @l@.
  --
  -- If @l@ lacks a battery, then @currBatVoltage k@ is 'Nothing'.
  currBatVoltage :: !(Maybe Double),
  -- | If @l@ has a battery, then @currBatVoltage k@ is 'Just' the
  -- rated voltage of the primary battery of @l@.
  --
  -- If @l@ lacks a battery, then @currBatVoltage k@ is 'Nothing'.
  ratedBatVoltage :: !(Maybe Double),
  -- | @loadAverage1Minute k@ is the one-minute load average of @l@.
  loadAverage1Minute :: !Double,
  -- | If @l@ has a battery, then @lowBatCapacity k@ is the maximum
  -- low capacity of the battery of @l@.
  --
  -- If @l@ does not have a battery, then @lowBatCapacity k@ is
  -- 'Nothing'.
  lowBatCapacity :: !(Maybe Double),
  -- | If @l@ has a battery, then @remBatCapacity k@ is the remaining
  -- capacity of the battery of @l@.
  --
  -- If @l@ does not have a battery, then @remBatCapacity k@ is
  -- 'Nothing'.
  remBatCapacity :: !(Maybe Double),
  -- | @numProcessors k@ is the number of on-line processors of @l@.
  --
  -- VARIK finds that storing a value which should be a natural number
  -- as a 'Double' feels a bit dirty... but is probably the best
  -- solution in this case; if 'numProcessors' is an 'Integral' type,
  -- then comparing 'numProcessors' and 'loadAverage1Minute' demands the
  -- conversion of 'numProcessors' into a 'Double', anyway.
  numProcessors :: Double
} deriving (Show);

-- | @nabSystemInfo@ returns 'SystemInfo' regarding the system on which
-- @nabSystemInfo@ is run.
nabSystemInfo :: IO SystemInfo;
#ifdef openbsd_HOST_OS
nabSystemInfo = infoToSystemInfo <$> getInfo
  where
  infoToSystemInfo [tmp, ratB, road, l1, numbHeart, lC, rC] = SystemInfo {
    temperature = mayB tmp + 273.15,
    -- \^ C-to-K conversion occurs here.
    ratedBatVoltage = ratB,
    currBatVoltage = road,
    loadAverage1Minute = mayB l1,
    lowBatCapacity = lC,
    remBatCapacity = rC,
    numProcessors = mayB numbHeart
  }
  -- \| The C preprocessor prevents the use of the standard
  -- backslash-based long string notation.
  -- This solution is a bit ugly but at least works.
  --
  -- [INSERT THE NOISE OF AN IRRITATED TIM ALLEN HERE.]
  mayB = fromMaybe (error $ "An error is revealed!  A value " ++
                            "is Nothing, which indicates that " ++
                            "sysctl(8)'s output is not parsed " ++
                            "successfully.");

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
  -- \| @sysctlNames@ is a 7-list.  The purposes of the elements of this
  -- list, in order, are as follows:
  --
  -- 1. The first element of the list is the sysctl(8)-friendly name of
  --    the temperature sensor.
  --
  -- 2. The second element of the list is the sysctl(8)-friendly name of
  --    the rated-battery-voltage-reporting thing.
  --
  -- 3. The third element of the list is the sysctl(8)-friendly name of
  --    the battery voltage sensor.
  --
  -- 4. The fourth element of the list is the sysctl(8)-friendly name of
  --    the thing what reports the system load average.  Unless Hell
  --    freezes over, this value is PROBABLY "vm.loadavg".
  --
  -- 5. The fifth element of the list is the sysctl(8)-friendly name of
  --    the thing which reports the number of on-line processors which
  --    the system contains.  For reasons which should be obvious, this
  --    value is generally "hw.ncpuonline".]
  --
  -- 6. The sixth element of the list is the sysctl(8)-friendly name of
  -- the thing which reports the "low" capacity of the battery.
  --
  -- 7. The seventh element of the list is the sysctl(8)-friendly name
  -- of the thing which reports the remaining capacity of the battery.
  sysctlNames = ["hw.sensors.cpu0.temp0",
                 "hw.sensors.acpibat0.volt0",
                 "hw.sensors.acpibat0.volt1",
                 "vm.loadavg",
                 "hw.ncpuonline",
                 "hw.sensors.acpibat0.amphour2",
                 "hw.sensors.acpibat0.amphour3"]
  -- \| sysctl(8) is used instead of sysctl(2) because the simplicity of
  -- using sysctl(8) is greater than the simplicity of using sysctl(2);
  -- the reading of sensors via sysctl(2) demands the use of some
  -- @struct@s and other crazy things, whereas sysctl(8)'s use just
  -- demands some basic parsing.
  --
  -- sysctl(8) is _technically_ relatively inefficient.  However, such
  -- inefficiency should be trivial.
  getValue a = maybeFirst <$> readProcessWithExitCode "sysctl" [a] []
    where
    maybeFirst (code, stdout, stderr) = case a of
      -- \| vm.loadavg contains a few values.  However, the first value
      -- is the only value which is terribly important.
      "vm.loadavg" -> (code, head $ words stdout, stderr)
      _            -> (code, stdout, stderr);

-- | Where @(a,b,c)@ is the output of a sysctl(8) command which is run
-- via @'readProcessWithExitCode'@, if @b@ contains a 'Double', then
-- @extractDoubleValue k@ 'Just' returns this 'Double'.
-- @extractDoubleValue k@ otherwise outputs 'Nothing'.
extractDoubleValue :: (ExitCode, String, String) -> Maybe Double;
extractDoubleValue (exitcode, stdout, stderr) = withExitCode exitcode
  where
  withExitCode (ExitFailure _) = Nothing
  withExitCode ExitSuccess = readMaybe $
    {-      @     @      -}  -- \| Whatever unit follows the space can
    {-       @   @       -}  -- be safely discarded; other parts of
    {-        @ @        -}  -- this program account for such units.
    {-         @         -}  head $ words $
    {- @               @ -}  -- \| Take the thing which FOLLOWS the
    {-  @@           @@  -}  -- equals sign, dumb-ass.
    {-    @@@@@@@@@@@    -}  (!!1) $ splitOn "=" stdout;
#else
nabSystemInfo = error "nabSystemInfo is unfamiliar with this \
                      \operating system.";
#endif
