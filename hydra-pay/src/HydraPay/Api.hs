{-# LANGUAGE TemplateHaskell #-}
-- |

module HydraPay.Api where

import Text.Printf
import HydraPay.PaymentChannel
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens hiding (argument)
import qualified Cardano.Api as Api
import HydraPay.Utils
import qualified Data.Aeson as Aeson
import Data.Int
import Data.Text (Text)
import qualified Data.Text as T
import HydraPay.Orphans ()

data InstanceRequest
  = CreatePaymentChannel Text Api.AddressAny Api.AddressAny
  | GetStatus Text
  | GetLock Text Api.AddressAny Int32
  | SendInChannel Text Api.AddressAny Int32
  | SubmitInChannel Text Api.AddressAny Text
  | CloseChannel Text
  deriving (Generic)

instance Aeson.ToJSON InstanceRequest
instance Aeson.FromJSON InstanceRequest

data DetailedStatus = DetailedStatus
  { _detailedStatus_first :: Api.AddressAny
  , _detailedStatus_second :: Api.AddressAny
  , _detailedStatus_firstBalance :: Int32
  , _detailedStatus_secondBalance :: Int32
  , _detailedStatus_transactions :: Map Int32 TransactionInfo
  }
  deriving (Generic)

instance Aeson.ToJSON DetailedStatus
instance Aeson.FromJSON DetailedStatus

data FundInternalWallet = FundInternalWallet
  { _fundInternal_internalWalletAddress :: Api.AddressAny
  , _fundInternal_txPayload :: Text
  }
  deriving (Generic)

instance Aeson.ToJSON FundInternalWallet
instance Aeson.FromJSON FundInternalWallet

data FundPaymentChannel = FundPaymentChannel
  { _fundChannel_amount :: Int32
  , _fundChannel_txPayload :: Text
  , _fundChannel_witnessPayload :: Text
  }
  deriving (Generic)

instance Aeson.ToJSON FundPaymentChannel
instance Aeson.FromJSON FundPaymentChannel

data LockResult
  = LockFundInternal FundInternalWallet
  | LockInChannel FundPaymentChannel
  | LockCreateOutput Int32 Text
  deriving (Generic)

instance Aeson.ToJSON LockResult
instance Aeson.FromJSON LockResult

data SendTxRaw = SendTxRaw
  { _sendTxRaw_from :: Api.AddressAny
  , _sendTxRaw_to :: Api.AddressAny
  , _sendTxRaw_amount :: Int32
  , _sendTxRaw_payload :: Text
  }
  deriving (Generic)

instance Aeson.ToJSON SendTxRaw
instance Aeson.FromJSON SendTxRaw

data InstanceResponse
  = NewPaymentChannel Text FundInternalWallet
  | InstanceError Text
  | ChannelStatus Text PaymentChannelStatus (Maybe DetailedStatus)
  | LockAttempt Text LockResult
  | SendTx Text SendTxRaw
  | SuccessMessage Text
  deriving (Generic)

makeLenses ''DetailedStatus
makeLenses ''FundInternalWallet

makeHumanReadable :: InstanceResponse -> Text
makeHumanReadable = \case
  NewPaymentChannel name (FundInternalWallet internalWallet txPayload) ->
    T.intercalate "\n" [ "New payment channel '" <> name <> "' created successfully"
                       , "please fund your Hydra Node's internal wallet, the address is of your internal wallet is: " <> Api.serialiseAddress internalWallet
                       , "enclosed is a transaction that will fund this internal wallet with 30ADA"
                       , txPayload
                       ]

  ChannelStatus name status mDetails -> case status of
    PaymentChannelStatus_Submitting -> name <> " is currently initializing, ensure you have funded an internal wallet"
    PaymentChannelStatus_WaitingForAccept -> name <> " is waiting for locked funds (do 'hydra-pay channel lock " <> name <> " <address>') to get a lock transaction"
    PaymentChannelStatus_Opening -> name <> " has locked funds, opening on L1..."
    PaymentChannelStatus_Open -> case mDetails of
      Just details -> do
        let
          addrFirstText = details ^. detailedStatus_first . to Api.serialiseAddress
          addrSecondText = details ^. detailedStatus_second . to Api.serialiseAddress
          addrFirstShort = T.takeEnd 8 addrFirstText
          addrSecondShort = T.takeEnd 8 addrSecondText

          balanceFirst = details ^. detailedStatus_firstBalance;
          balanceSecond = details ^. detailedStatus_secondBalance;
          txns = details ^. detailedStatus_transactions

          renderTxn :: TransactionInfo -> Text
          renderTxn ti = do
            T.intercalate " "
              [ tShow (ti ^. transactionInfo_time)
              , tShow (ti ^. transactionInfo_amount)
              , case (ti ^. transactionInfo_direction) of
                  TransactionReceived -> addrFirstShort <>  " <- " <> addrSecondShort
                  TransactionSent -> addrFirstShort <>  " -> " <> addrSecondShort
              ]

        T.intercalate "\n" $
          [ " --- Open Payment Channel '" <> name <> "' ---"
          , addrFirstShort <> " has a balance of " <> (T.pack $ printf "%.2f" $ ((fromIntegral balanceFirst :: Double) / 1000000.0))
          , addrSecondShort <> " has a balance of " <> (T.pack $ printf "%.2f" $ ((fromIntegral balanceSecond :: Double) / 1000000.0))
          ] <> (if Map.null txns then [] else (["Transactions:"] <> (fmap renderTxn $ reverse $ Map.elems txns)))

      Nothing -> name <> " is open"
    PaymentChannelStatus_Closing -> name <> " is closing"
    PaymentChannelStatus_Error -> name <> " is in a failure state"

  InstanceError msg -> msg
  SuccessMessage msg -> msg
  SendTx _ (SendTxRaw fromAddr toAddr amount payload) ->
    T.intercalate "\n" [ "Here is a transaction to send " <> formatLovelaceAsAda amount <> "ADA from " <> fromAddrShort <> " to " <> toAddrShort <> " in your payment channel"
                       , payload
                       ]
    where
      fromAddrShort = toShortAddress fromAddr
      toAddrShort = toShortAddress toAddr
  LockAttempt _ result ->
    case result of
      LockFundInternal (FundInternalWallet addr payload) ->
        T.intercalate "\n" [ "Please fund your Hydra Node's internal wallet, the address is " <> Api.serialiseAddress addr
                           , "enclosed is a transaction that will fund this internal wallet with 30ADA"
                           , payload
                           ]

      LockCreateOutput amount payload ->
        T.intercalate "\n"
          [ "This address doesn't have a UTxO with exactly " <> formatLovelaceAsAda amount <> "ADA"
          , "Here is a transaction that will create a UTxO with exactly " <> formatLovelaceAsAda amount <> " submit this, then run this command again."
          , "(If you recently submitted such a transaction please wait for it to appear on-chain)"
          , payload
          ]

      LockInChannel (FundPaymentChannel amount cbor witness) ->
        T.intercalate "\n"
          [ "Here is a transaction to lock " <> formatLovelaceAsAda amount <> "ADA into this payment channel"
          , ""
          , cbor
          , ""
          , "Here is the witness from your Hydra Node's internal wallet, you may need this depending on how you sign and submit transactions:"
          , witness
          ]

formatLovelaceAsAda :: Int32 -> Text
formatLovelaceAsAda amount = T.pack $ printf "%.2f" $ (fromIntegral amount :: Double) / 1000000.0

toShortAddress :: Api.AddressAny -> Text
toShortAddress = T.takeEnd 8 . Api.serialiseAddress

instance Aeson.ToJSON InstanceResponse
instance Aeson.FromJSON InstanceResponse
