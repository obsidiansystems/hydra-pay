-- | 

module HydraPay.WebSocket where

import Hydra.Types
import Hydra.ClientInput
import Hydra.ServerOutput
import Hydra.Devnet

import HydraPay

import Data.IORef
import Data.Aeson as Aeson
import GHC.Generics

import Control.Monad (forever)
import Control.Monad.Loops (untilJust)

import qualified Network.WebSockets as WS
import HydraPay.Api

data Tagged a = Tagged
  { tagged_id :: Int
  , tagged_payload :: a
  }
  deriving (Eq, Show, Generic)

instance ToJSON a => ToJSON (Tagged a)
instance FromJSON a => FromJSON (Tagged a)

data ClientMsg
  = ClientHello

  | CreateHead HeadCreate
  | InitHead HeadInit
  | CommitHead HeadCommit
  | CloseHead HeadName

  | CheckFuel Address
  | Withdraw Address
  | GetAddTx TxType Address Lovelace
  deriving (Eq, Show, Generic)

instance ToJSON ClientMsg
instance FromJSON ClientMsg

data ServerMsg
  = ServerHello
  | FundsTx Tx
  | FuelAmount Lovelace
  | OperationSuccess
  | InvalidMessage
  | UnhandledMessage
  | ServerError HydraPayError
  deriving (Eq, Show, Generic)

instance ToJSON ServerMsg
instance FromJSON ServerMsg

handleTaggedMessage :: State -> Tagged ClientMsg -> IO (Tagged ServerMsg)
handleTaggedMessage state (Tagged tid msg) = do
  Tagged tid <$> handleClientMessage state msg

handleClientMessage :: State -> ClientMsg -> IO ServerMsg
handleClientMessage state = \case
  ClientHello -> pure ServerHello

  Withdraw address -> do
    -- Withdraw everything except the minRemainder
    amount <- getProxyFunds state address
    result <- withdraw state $ WithdrawRequest address (amount - minRemainder)
    case result of
      Right txid -> withLogging $ waitForTxIn (_cardanoNodeInfo . _state_hydraInfo $ state) $ txInput 0 txid
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
    pure OperationSuccess

  _ -> pure UnhandledMessage
  where
    minRemainder = 3000000

-- Integration tests for the various messages
testSayHello :: IO ()
testSayHello = do
  WS.runClient "localhost" 8000 "hydra/api" $ \conn -> do
    WS.sendTextData conn . Aeson.encode $ Tagged 0 ClientHello

    msg <- Aeson.decode <$> WS.receiveData conn
    case msg of
      Just (Tagged 0 ServerHello) -> putStrLn "Server says hello"
      _ -> putStrLn ""
    pure ()

runHydraPayClient :: (WS.Connection -> IO a) -> IO a
runHydraPayClient action = do
  WS.runClient "localhost" 8000 "hydra/api" action

requestResponse :: WS.Connection -> ClientMsg -> IO (Maybe ServerMsg)
requestResponse conn msg = do
  WS.sendTextData conn . Aeson.encode $ msg
  Aeson.decode <$> WS.receiveData conn

getAddFundsTx :: Address -> Lovelace -> IO (Maybe Tx)
getAddFundsTx addr amount = do
  WS.runClient "localhost" 8000 "hydra/api" $ \conn -> do
    WS.sendTextData conn . Aeson.encode $ Tagged 0 $ GetAddTx Funds addr amount

    result <- untilJust $ do
      msg <- Aeson.decode <$> WS.receiveData conn
      case msg of
        Just (Tagged 0 (FundsTx tx)) -> pure $ Just $ Just tx
        Just (Tagged 0 _) -> pure $ Just Nothing
        Just _ -> pure Nothing

    pure $ result
