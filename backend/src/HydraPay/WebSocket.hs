-- | 

module HydraPay.WebSocket where

import Hydra.Types
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
  | GetAddFundsTx Address Lovelace
  deriving (Eq, Show, Generic)

instance ToJSON ClientMsg
instance FromJSON ClientMsg

data ServerMsg
  = ServerHello
  | FundsTx Tx
  | InvalidMessage
  | UnhandledMessage
  | InternalError HydraPayError
  deriving (Eq, Show, Generic)

instance ToJSON ServerMsg
instance FromJSON ServerMsg

handleTaggedMessage :: State -> Tagged ClientMsg -> IO (Tagged ServerMsg)
handleTaggedMessage state (Tagged tid msg) = do
  Tagged tid <$> handleClientMessage state msg

handleClientMessage :: State -> ClientMsg -> IO ServerMsg
handleClientMessage state = \case
  ClientHello -> pure ServerHello
  GetAddFundsTx addr amount -> do
    result <- buildAddTx Funds state addr amount
    pure $ either InternalError FundsTx result
  _ -> pure UnhandledMessage

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

data ClientState = ClientState
  { clientState_nextId :: IORef Int
  , clientState_conn :: WS.Connection
  }

getNextId :: ClientState -> IO Int
getNextId (ClientState _ _) = pure 1

{-newtype ClientT m a = ClientT
  { runClientT :: ClientState -> ReaderT ClientState m a
  }-}

getAddFundsTx :: Address -> Lovelace -> IO (Maybe Tx)
getAddFundsTx addr amount = do
  WS.runClient "localhost" 8000 "hydra/api" $ \conn -> do
    WS.sendTextData conn . Aeson.encode $ Tagged 0 $ GetAddFundsTx addr amount

    result <- untilJust $ do
      msg <- Aeson.decode <$> WS.receiveData conn
      case msg of
        Just (Tagged 0 (FundsTx tx)) -> pure $ Just $ Just tx
        Just (Tagged 0 _) -> pure $ Just Nothing
        Just _ -> pure Nothing

    pure $ result
