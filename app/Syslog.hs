{-# LANGUAGE CApiFFI #-}

-- | Module    : Syslog
-- Description : albertlertd(8)'s interface to syslogd(8)
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contans 'syslog', which writes (to the system
-- log) messages.
module Syslog (syslog) where
import Foreign.C.Error (throwErrnoIfMinus1_);
import Foreign.C.String (CString, withCString);
import Foreign.C.Types;

foreign import capi "syslog.h syslog"
  syslog_c :: CInt
           -> CString
           -> CString
           -> IO ();

foreign import capi "syslog.h value LOG_WARNING"
  logwarning :: CInt;

-- | @syslog l@ writes (to the system log and as a daemon) @l@.
syslog :: String -> IO ();
syslog l = withCString l $ \mosig ->
           -- \| @shutUp@ silences a "potentially insecure" warning
           -- message.
           withCString "%s" $ \shutUp ->
           syslog_c logwarning shutUp mosig;
