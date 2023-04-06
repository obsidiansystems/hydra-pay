module HydraPay.Watch where

import Control.Monad
import Control.Concurrent
import Control.Monad.IO.Class
import Reflex
import Reflex.FSNotify hiding (wrapWatch, watchDir)
import qualified System.FSNotify as FS

import HydraPay.Host

wrapWatch
  :: (MonadIO m, TriggerEvent t m)
  => (FS.WatchManager -> pathinfo -> FS.Action -> IO a)
  -> FS.WatchConfig
  -> pathinfo
  -> m (Event t FSEvent)
wrapWatch f cfg path = do
  (ev, fire) <- newTriggerEvent
  void $ liftIO $ forkIO $ FS.withManagerConf cfg $ \mgr -> do
    _ <- f mgr path fire
    forever $ threadDelay $ 1000 * 1000
  pure ev

watchDir
  :: (TriggerEvent t m, MonadIO m)
  => FS.WatchConfig
  -> FilePath
  -> FS.ActionPredicate
  -> m (Event t FSEvent)
watchDir cfg path evFilter = wrapWatch (\mgr p action -> FS.watchDir mgr p evFilter action) cfg path
