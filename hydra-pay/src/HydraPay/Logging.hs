-- |

module HydraPay.Logging ( LogMessage(..)
                        , LogLevel(..)
                        , Logger
                        , LogConfig(..)
                        , HasLogger(..)

                        , defaultLogConfig
                        , withLogger

                        , logM
                        , logDebug
                        , logInfo
                        , logWarn
                        , logError
                        , logCrit
                        ) where

import Data.Int
import Data.IORef
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T

import Data.Time

import System.IO
import System.Directory

import Control.Lens

import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception hiding (handle)

import GHC.Natural

data Logger = Logger (TBQueue LogMessage)

class HasLogger a where
  getLogger :: Lens' a Logger

instance HasLogger Logger where
  getLogger = id

data LogLevel =
  Critical | Error | Warning | Info | Debug
  deriving stock (Eq, Show, Enum)

data LogMessage = LogMessage
  { logMessage_level :: LogLevel
  , logMessage_prefix :: Text
  , logMessage_contents :: Text
--  , logMessage_time :: UTCTime
  }

data LogRotationSchedule = Daily

data LogConfig = LogConfig
  { logConfig_maxFileSize :: Integer -- Bytes
  , logConfig_maxLogFiles :: Maybe Int32
  , logConfig_rotationSchedule :: LogRotationSchedule
  , logConfig_appName :: Text
  , logConfig_path :: FilePath
  , logConfig_maxQueueSize :: Natural
  }

defaultLogConfig :: Text -> LogConfig
defaultLogConfig name =
  LogConfig (1000000) Nothing Daily name "./logs" 1000

getLocalTime :: IO LocalTime
getLocalTime = utcToLocalTime <$> getCurrentTimeZone <*> getCurrentTime

getLogFilePath :: Int -> LocalTime -> LogConfig -> IO FilePath
getLogFilePath count localTime ls@(LogConfig maxSize _ _ name path _) = do
  createDirectoryIfMissing True path
  let
    timeStr = formatTime defaultTimeLocale "%F" localTime
    countStr =
      case count of
        0 -> ""
        n -> "_" <> show n

    logFile = path <> "/" <> T.unpack name <> ".log." <> timeStr <> countStr
  exists <- doesFileExist logFile
  case exists of
    True -> do
      logSize <- getFileSize logFile
      case logSize >= maxSize of
        True -> getLogFilePath (count + 1) localTime ls
        False -> pure logFile
    False -> pure logFile

aquireLogFile :: LogConfig -> LocalTime -> IO (Handle, FilePath)
aquireLogFile ls localTime = do
  fp <- getLogFilePath 0 localTime ls
  handle <- openFile fp AppendMode
  pure (handle, fp)

renderLog :: LogMessage -> T.Text
renderLog (LogMessage l p c) =
  "[" <> (T.pack . show) l <> "] " <> p <> " - " <> c <> "\n"

logReader :: LogConfig -> TBQueue LogMessage -> IO ()
logReader settings queue = do
  time <- getLocalTime
  let
    getLogFile = do
      logFile <- aquireLogFile settings time
      logFileRef <- newIORef logFile
      pure logFileRef
  bracket getLogFile (\ref -> do
                       (h, _) <- readIORef ref
                       hClose h) $ \logFileRef -> do
    forever $ do
      (fileHandle, filePath) <- readIORef logFileRef
      msg <- atomically $ readTBQueue queue
      let
        handle = logHandle $ logMessage_level msg
        renderered = renderLog msg

      -- Log to the std stream and file handle
      T.hPutStr handle renderered
      T.hPutStr fileHandle renderered

      -- Check if we should rotate, because enough time has passed, or the log is too big
      currentTime <- getLocalTime
      let
        isSameDay = diffDays (localDay time) (localDay currentTime) == 0

      logSize <- getFileSize filePath
      when (not isSameDay || logSize >= logConfig_maxFileSize settings) $ do
        newLogFile <- aquireLogFile settings currentTime
        hClose fileHandle
        writeIORef logFileRef newLogFile
      pure ()

withLogger :: LogConfig -> (Logger -> IO a) -> IO a
withLogger settings action = do
  queue <- newTBQueueIO $ logConfig_maxQueueSize settings
  bracket (forkIO $ logReader settings queue) killThread $ \_ -> do
    action $ Logger queue

logM :: (MonadIO m, HasLogger a) => a -> LogLevel -> Text -> Text -> m ()
logM a level subsystem msg = liftIO $ do
  atomically $ writeTBQueue q logMessage
  where
    Logger q = a ^. getLogger
    logMessage = LogMessage level subsystem msg

logInfo :: (MonadIO m, HasLogger a) => a -> Text -> Text -> m ()
logInfo l subsystem msg = do
  logM l Info subsystem msg

logDebug :: (MonadIO m, HasLogger a) => a -> Text -> Text -> m ()
logDebug l subsystem msg = do
  logM l Debug subsystem msg

logError :: (MonadIO m, HasLogger a) => a -> Text -> Text -> m ()
logError l subsystem msg = do
  logM l Error subsystem msg

logCrit :: (MonadIO m, HasLogger a) => a -> Text -> Text -> m ()
logCrit l subsystem msg = do
  logM l Critical subsystem msg

logWarn :: (MonadIO m, HasLogger a) => a -> Text -> Text -> m ()
logWarn l subsystem msg = do
  logM l Warning subsystem msg

logHandle :: LogLevel -> Handle
logHandle Error = stderr
logHandle Critical = stderr
logHandle _ = stdout
