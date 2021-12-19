{-# LANGUAGE CApiFFI #-}

-- | Module    : Syslog
-- Description : albertlertd(8)'s interface to syslogd(8)
-- Copyright   : (c) Varik Valefor, 2021
-- License     : Unlicense
-- Maintainer  : varikvalefor@aol.com
-- Stability   : experimental
-- Portability : POSIX
--
-- This module contans 'syslog', which writes messages to the system
-- log.
module Syslog where
import Foreign.C.Error (throwErrnoIfMinus1_);
import Foreign.C.String (CString, withCString);
import Foreign.C.Types;

foreign import capi "syslog.h syslog"
  syslog_c :: CInt
           -> CString
           -> CString
           -> IO ();

foreign import capi "syslog.h value LOG_DAEMON"
  logdaemon :: CInt;

-- | @syslog l@ writes @l@ to the system log as a daemon.
syslog :: String -> IO ();
syslog l = withCString l $ \mosig ->
           -- \| @shutUp@ silences a "potentially insecure" warning
           -- message.
           withCString "%s" $ \shutUp ->
           syslog_c logdaemon shutUp mosig;
