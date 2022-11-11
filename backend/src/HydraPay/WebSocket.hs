{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

-- | 

module HydraPay.WebSocket where


import Hydra.Types
import Hydra.ClientInput
import Hydra.ServerOutput
import Hydra.Devnet

import HydraPay
import HydraPay.Api

import qualified Data.Text as T
import Data.IORef
import Data.Foldable
import Data.Aeson as Aeson
import GHC.Generics

import qualified Data.Map as Map

import qualified Network.WebSockets as WS
import HydraPay.Api
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Lens

import Control.Concurrent
import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TChan
import Control.Monad (forever)
import Control.Monad.Loops (untilJust)

import Control.Monad.Reader
import Control.Monad.Fail
import Control.Monad.Trans.Maybe
import Control.Applicative

import System.Process
import System.Directory

import qualified Network.WebSockets as WS
import HydraPay.Api
import Hydra.Devnet

handleTaggedMessage :: WS.Connection -> State -> Tagged ClientMsg -> IO (Tagged ServerMsg)
handleTaggedMessage conn state (Tagged tid msg) = do
  msg <- handleClientMessage conn state msg
  pure $ Tagged tid msg

versionStr :: Version
versionStr = "0.1.0"

handleClientMessage :: WS.Connection -> State -> ClientMsg -> IO (ServerMsg)
handleClientMessage conn state = \case
  RestartDevnet -> do
    cns <- readMVar (_state_cardanoNodeState state)
    case cardanoNodeState_nodeType cns of
      CfgPreview _ -> pure ()
      CfgDevnet -> do
        heads <- Map.keys <$> readMVar (_state_heads state)
        -- Terminate all the heads!
        for_ heads $ terminateHead state

        -- Create a new devnet and update the MVar
        modifyMVar_ (_state_cardanoNodeState state) $ \cns -> do
          teardownDevnet cns
          withLogging getCardanoNodeState
    pure DevnetRestarted

  GetDevnetAddresses amount -> do
    mAddrs <- getDevnetAddresses [1 .. amount]
    case mAddrs of
      Just addrs ->
        pure $ DevnetAddresses addrs
      Nothing ->
        pure $
        RequestError
        "Unable to open seeded address file, restart devnet or wait for seeding to complete"

  SubscribeTo name -> do
    modifyMVar_ (state ^. state_subscribers) $ pure . Map.insertWith (<>) name (pure conn)
    pure $ SubscriptionStarted name

  ClientHello -> pure $ ServerHello versionStr

  Withdraw address -> do
    -- Withdraw everything minus fees
    result <- withdraw state $ WithdrawRequest address Nothing
    case result of
      Right txid -> do
        cardanoNodeInfo <- stateCardanoNodeInfo state
        withLogging $ waitForTxIn cardanoNodeInfo $ txInput 0 txid
      _ -> pure ()
    pure $ either ServerError (const OperationSuccess) result

  GetAddTx txtype addr amount -> do
    result <- buildAddTx txtype state addr amount
    pure $ either ServerError FundsTx result

  CheckFuel addr -> do
    -- Calc the fuel amount
    fuel <- getProxyFuel state addr
    pure $ FuelAmount fuel

  CreateHead hc -> do
    result <- createHead state hc
    pure $ either ServerError (const OperationSuccess) result

  InitHead hi -> do
    result <- initHead state hi
    pure $ either ServerError (const OperationSuccess) result

  CommitHead hc -> do
    result <- commitToHead state hc
    pure $ either ServerError (const OperationSuccess) result

  CloseHead name -> do
    sendToHeadAndWaitFor Close (\case
      HeadIsFinalized {} -> True
      _ -> False) state name
    pure $ OperationSuccess

  DoesHeadExist name -> do
    result <- getHeadStatus state name
    pure $ HeadExistsResult $ case result of
      Left _ -> False
      Right _ -> True

  TearDownHead name -> do
    removeHead state name
    pure $ OperationSuccess

  _ -> pure $ UnhandledMessage
  where
    minRemainder = 3000000

newtype HydraPayClient a = HydraPayClient
  { unHydraPayClient :: MaybeT (ReaderT ClientState IO) a
  }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail)

data ClientState = ClientState
  { clientState_connection :: WS.Connection
  , clientState_inbox :: TChan Msg
  , clientState_msgs :: TChan ServerMsg
  , clientState_nextId :: MVar Int
  }

data Msg
  = TaggedMsg (Tagged ServerMsg)
  | PlainMsg ServerMsg

instance FromJSON Msg where
  parseJSON v = (TaggedMsg <$> parseJSON v) <|> (PlainMsg <$> parseJSON v)

runHydraPayClient :: HydraPayClient a -> IO (Maybe a)
runHydraPayClient action = do
  nextId <- newMVar 0
  broadcastChannel <- newBroadcastTChanIO
  msgsChannel <- newBroadcastTChanIO
  WS.runClient "127.0.0.1" 8000 "hydra/api" $ \conn -> do
    -- We have a read thread that is centralized so we don't miss any messages
    _ <- forkIO $ forever $ do
      mMsg :: Maybe Msg <- Aeson.decode <$> WS.receiveData conn
      case mMsg of
        Just (PlainMsg p) -> do
          atomically $ writeTChan msgsChannel p
        Just msg ->
          atomically $ writeTChan broadcastChannel msg
        Nothing -> pure ()
      pure ()
    flip runReaderT (ClientState conn broadcastChannel msgsChannel  nextId) $ runMaybeT $ unHydraPayClient action

requestResponse :: ClientMsg -> HydraPayClient ServerMsg
requestResponse msg = HydraPayClient . MaybeT . ReaderT $ \(ClientState conn inbox otherInbox nid) -> do
  n <- modifyMVar nid $ \x -> pure (x + 1, x)
  readChan <- atomically $ dupTChan inbox
  WS.sendTextData conn . Aeson.encode $ Tagged n msg
  Just <$> waitForResponse n readChan
  where
    waitForResponse n chan = do
      msg <- atomically $ readTChan chan
      case msg of
        TaggedMsg (Tagged n' msg) | n == n' -> pure msg
        _ -> waitForResponse n chan

-- Start commiting
-- Some time after we start commiting, we will get a HeadIsOpen
-- at that point the Head is ready to go
-- We need to wait for this HeadIsOpen or some error or something
waitForHeadOpen :: HeadName -> HydraPayClient ()
waitForHeadOpen hname = HydraPayClient . MaybeT . ReaderT $ \(ClientState _ _ box nid) -> do
  putStrLn "Waiting for OPEN"
  readChan <- atomically $ dupTChan box
  checkForOpenStatus readChan
  liftIO $ putStrLn "DONE WE ARE OPEN"
  Just <$> pure ()
  where
    checkForOpenStatus box = do
      putStrLn "Waiting for a thing??"
      msg <- atomically $ readTChan box
      putStrLn "Did we even get this??"
      case msg of
        HeadStatusChanged name Status_Open | name == hname -> pure ()
        _ -> checkForOpenStatus box

getAddFundsTx :: Address -> Lovelace -> IO (Maybe Tx)
getAddFundsTx addr amount = do
  result <- runHydraPayClient $ requestResponse $ GetAddTx Funds addr amount
  pure $ case result of
    Just (FundsTx tx) -> Just tx
    _ -> Nothing
