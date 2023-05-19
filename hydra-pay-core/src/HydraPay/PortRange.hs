{-# LANGUAGE TemplateHaskell #-}

module HydraPay.PortRange ( PortRange
                          , Port
                          , mkPortRange
                          , isPortFree
                          , allocatePorts
                          , allocatedPorts
                          , freePortAllocation
                          , HasPortRange(..)
                          ) where

import Data.Word
import Data.Set (Set)
import qualified Data.Set as Set

import Control.Lens
import Control.Monad
import Control.Exception
import Control.Monad.IO.Class
import Control.Concurrent.STM
import Control.Monad.Error.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Except
import Control.Monad.Trans.State (StateT(..))

import HydraPay.Utils
import HydraPay.Logging

import Data.Text (Text)
import qualified Data.Text as T

import System.IO.Error
import Network.Socket

type Port = Word16

data PortRange = PortRange
  { _portRange_ports :: TMVar (Set Port)
  }

makeLenses ''PortRange

class HasPortRange a where
  portRange :: Lens' a PortRange

instance HasPortRange PortRange where
  portRange = id

data PortAllocation = PortAllocation
  { _portAllocation_free :: IO ()
  -- ^ Finalizer that returns the ports back to the PortRange they came from
  , _portAllocation_ports :: [Port]
  }

makeLenses ''PortAllocation

type PortT m a = ExceptT Text (StateT [Port] m) a

mkPortRange :: MonadIO m => [Port] -> m PortRange
mkPortRange = liftIO . fmap PortRange . newTMVarIO . Set.fromList

allocatedPorts :: PortAllocation -> [Port]
allocatedPorts = view portAllocation_ports

freePortAllocation :: PortAllocation -> IO ()
freePortAllocation = view portAllocation_free

isPortFree :: MonadIO m => Port -> m Bool
isPortFree port = liftIO $ do
  let hints = defaultHints { addrFlags = [AI_PASSIVE], addrSocketType = Stream }
  addr:_ <- getAddrInfo (Just hints) Nothing (Just $ show port)
  bracket (socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr))
          close
          (\sock -> do
             setSocketOption sock ReuseAddr 1
             bind sock (addrAddress addr)
             return True)
   `catch` (\e -> if isAlreadyInUseError e then return False else ioError e)

allocatePorts :: (HasLogger a, MonadIO m) => a -> PortRange -> Int -> m (Either Text PortAllocation)
allocatePorts a range n = do
  logInfo a "allocatePorts" $ "Getting " <> tShow n <> " ports from PortRange"
  result <- withTMVar (range ^. portRange_ports) $ \ports -> do
    (result, remaining) <- runPortT (Set.toList ports) $ takePorts a n
    pure (Set.fromList remaining, (\x -> PortAllocation (freePorts a range x) x) <$> result)
  case result of
    Left err ->
      logInfo a "allocatePorts" $ "Failed to allocate ports from PortRange: " <> err
    Right pa ->
      logInfo a "allocatePorts" $ "Allocated " <> T.intercalate ", " (pa ^. portAllocation_ports ^.. to tShow ) <> " from PortRange"
  pure result

freePorts :: HasLogger a => a -> PortRange -> [Port] -> IO ()
freePorts a range ports = do
  logInfo a "freePorts" $ "Returning ports: " <> T.intercalate ", " (fmap tShow ports) <> " ports to this PortRange"
  _ <- withTMVar (range ^. portRange_ports) $ \allPorts -> pure (allPorts <> Set.fromList ports, ())
  pure ()

runPortT :: [Port] -> PortT m a -> m (Either Text a, [Port])
runPortT ports action = flip runStateT ports $ runExceptT action

takePort :: (HasLogger a, MonadState [Port] m, MonadIO m, MonadError Text m) => a -> m Port
takePort a = do
  ports <- get
  case ports of
    [] -> do
      logError a "takePort" $ "We are out of free ports, ports list exhausted, ensure you are returning ports to the PortRange"
      throwError "PortRange exhausted"
    (port:rest) -> do
      put rest
      free <- liftIO $ isPortFree port
      case free of
        True -> do
          pure port
        False -> do
          logWarn a "takePort" $ "Port " <> tShow port <> " was already bound despite being in the free list"
          takePort a

takePorts :: (HasLogger a, MonadState [Port] m, MonadIO m, MonadError Text m) => a -> Int -> m [Port]
takePorts a n = replicateM n (takePort a)
