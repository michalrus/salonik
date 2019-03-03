module Salonik.Log
  ( debug
  , info
  , warn
  , error
  ) where

import qualified Data.Text             as T
import qualified Data.Time.Format      as TF
import           GHC.Conc.Sync         (myThreadId)
import           Prelude               hiding (error, log)
import qualified System.Console.ANSI   as ANSI
import           System.Environment    (lookupEnv)
import qualified System.IO             as IO
import           System.IO.Unsafe      (unsafePerformIO)
import           System.Log.FastLogger

{-# ANN module
          ("HLint: ignore Avoid restricted function" :: String)
        #-}

data Internal = Internal
  { logSet :: LoggerSet
  , inSystemd :: Bool
  , supportsANSI :: Bool
  }

internal :: IORef Internal
{-# NOINLINE internal #-}
internal =
  unsafePerformIO $ do
    logSet <- newStdoutLoggerSet 0 -- bufsize == 0, to always flush
    inSystemd <- isJust <$> lookupEnv "NOTIFY_SOCKET"
    supportsANSI <- ANSI.hSupportsANSI IO.stdout
    newIORef Internal {..}

-- |TODO: Maybe output straight to stdout, but via a TChan for concurrency? Where to start the consumer then?
-- |TODO: Maybe allow writing straight to 'syslog(3)', to have proper multiline messages?
-- |TODO: Or maybe straight to journal, to add code locations from 'HasCallStack'?
log :: Text -> Text -> [ANSI.SGR] -> Text -> IO ()
log systemdLevel humanLevel sgr msg = do
  Internal {..} <- readIORef internal
  prefix <-
    if inSystemd
      then pure systemdLevel
      else do
        now <- getCurrentTime
        threadId <- myThreadId
        pure $
          (if supportsANSI
             then pack (ANSI.setSGRCode (ANSI.Reset : sgr))
             else "") ++
          "[" ++
          pack (TF.formatTime TF.defaultTimeLocale "%FT%T%6QZ" now) ++
          "] [" ++ humanLevel ++ "] [" ++ tshow threadId ++ "] "
  let suffix :: Text
        | inSystemd = ""
        | supportsANSI = pack (ANSI.setSGRCode [ANSI.Reset])
        | otherwise = ""
  pushLogStr logSet $
    toLogStr prefix ++ toLogStr (T.replace "\n" ("\n" ++ prefix) msg) ++ toLogStr suffix ++ "\n"

debug :: Text -> IO ()
debug = log "<7>" "DEBUG" [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Blue]

info :: Text -> IO ()
info = log "<6>" "INFO " []

warn :: Text -> IO ()
warn = log "<4>" "WARN " [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Yellow]

error :: Text -> IO ()
error =
  log
    "<3>"
    "ERROR"
    [ANSI.SetColor ANSI.Foreground ANSI.Vivid ANSI.Red, ANSI.SetConsoleIntensity ANSI.BoldIntensity]
