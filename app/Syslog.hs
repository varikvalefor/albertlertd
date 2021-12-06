{-# LANGUAGE CApiFFI #-}

-- | Module    : Syslog
-- Description : albertlertd(8)'s interface to syslogd(8)
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : POSIX
module Syslog where
import Foreign.C.Error (throwErrnoIfMinus1_);
import Foreign.C.String (CString, withCString);
import Foreign.C.Types;

foreign import capi "syslog.h syslog"
  syslog_c :: CInt
           -> CString
           -> IO ();

foreign import capi "syslog.h value LOG_DAEMON"
  logdaemon :: CInt;

-- | @syslog l@ writes @l@ to the system log as a daemon.
syslog :: String -> IO ();
syslog l = withCString l (syslog_c logdaemon);
