module HydraPay.Host where

import Control.Concurrent.Chan (newChan, readChan)
import Control.Monad
import Control.Monad.Identity
import Control.Monad.IO.Class
import Data.Dependent.Sum (DSum ((:=>)))
import Data.IORef (readIORef)
import Data.Maybe
import Reflex
import Reflex.Host.Class

runHost :: (forall t m. MonadR t m => m ()) -> IO ()
runHost action =
  runSpiderHost $ do
    events <- liftIO newChan
    (_, fc) <- do
      hostPerformEventT $ do
        flip runTriggerEventT events $ do
          action
    fix $ \loop -> do
      ers <- liftIO $ readChan events
      _ <- fireEventTriggerRefs fc ers $ pure ()
      loop

type MonadR t m =
  ( MonadIO (Performable m)
  , MonadIO m
  , PerformEvent t m
  , TriggerEvent t m
  )

fireEventTriggerRefs :: (MonadIO m) => FireCommand t m -> [DSum (EventTriggerRef t) TriggerInvocation] -> ReadPhase m a -> m [a]
fireEventTriggerRefs (FireCommand fire) ers rcb = do
  mes <- liftIO $
    forM ers $ \(EventTriggerRef er :=> TriggerInvocation a _) -> do
      me <- readIORef er
      return $ fmap (\e -> e :=> Identity a) me
  a <- fire (catMaybes mes) rcb
  liftIO $ forM_ ers $ \(_ :=> TriggerInvocation _ cb) -> cb
  return a
