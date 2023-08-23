{-# LANGUAGE TemplateHaskell #-}
-- |

module HydraPay.Api where

import Data.Traversable
import Text.Printf
import HydraPay.PaymentChannel
import GHC.Generics (Generic)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Lens hiding (argument, (.=))
import qualified Cardano.Api as Api
import HydraPay.Utils
import Data.Aeson (object, (.=), (.:), (.:?))
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
  | RemoveChannel Text
  deriving (Generic)

instance Aeson.ToJSON InstanceRequest where
  toJSON = \case
    CreatePaymentChannel name p1 p2 ->
      object [ mkTag "create"
             , "name" .= name
             , "participants" .= [p1, p2]
             ]

    GetStatus name ->
      object [ mkTag "status"
             , "name" .= name
             ]

    GetLock name addr lovelace ->
      object [ mkTag "lock"
             , "name" .= name
             , "address" .= addr
             , "lovelace" .= lovelace
             ]
    SendInChannel name addr lovelace ->
      object [ mkTag "send"
             , "name" .= name
             , "address" .= addr
             , "lovelace" .= lovelace
             ]

    SubmitInChannel name addr signedTx ->
      object [ mkTag "submit"
             , "name" .= name
             , "address" .= addr
             , "signed-tx" .= signedTx
             ]
    CloseChannel name ->
      object [ mkTag "close"
             , "name" .= name
             ]
    RemoveChannel name ->
      object [ mkTag "remove"
             , "name" .= name
             ]

instance Aeson.FromJSON InstanceRequest where
  parseJSON = Aeson.withObject "Instance Request" $ \o -> do
    tag :: T.Text <- o .: "tag"
    case tag of
      "create" -> do
        name <- o .: "name"
        [a,b] <- o .: "participants"
        pure $ CreatePaymentChannel name a b
      "status" -> do
        name <- o .: "name"
        pure $ GetStatus name
      "lock" -> do
        name <- o .: "name"
        addr <- o .: "address"
        lovelace <- o .: "lovelace"
        pure $ GetLock name addr lovelace
      "send" -> do
        name <- o .: "name"
        addr <- o .: "address"
        lovelace <- o .: "lovelace"
        pure $ SendInChannel name addr lovelace
      "submit" -> do
        name <- o .: "name"
        address <- o .: "address"
        signedTx <- o .: "signed-tx"
        pure $ SubmitInChannel name address signedTx
      "close" -> do
        name <- o .: "name"
        pure $ CloseChannel name
      "remove" -> do
        name <- o .: "name"
        pure $ RemoveChannel name
      t -> fail $ "Encountered Invalid mkTag " <> T.unpack t

data DetailedStatus = DetailedStatus
  { _detailedStatus_first :: Api.AddressAny
  , _detailedStatus_second :: Api.AddressAny
  , _detailedStatus_firstBalance :: Int32
  , _detailedStatus_secondBalance :: Int32
  , _detailedStatus_transactions :: Map Int32 TransactionInfo
  }
  deriving (Generic)

instance Aeson.ToJSON DetailedStatus where
  toJSON (DetailedStatus f s fb sb ts) =
    object [ "participants" .= [particpantObj f fb, particpantObj s sb]
           , "transactions" .= (Map.elems . fmap transactionObj $ ts)
           ]
    where
      transactionObj :: TransactionInfo -> Aeson.Value
      transactionObj (TransactionInfo tId time amount dir) =
        object [ "id" .=  tId
               , "to" .= toAddr
               , "from" .= fromAddr
               , "lovelace" .= amount
               , "time" .= time
               ]
        where
          (toAddr, fromAddr) =
            case dir of
              TransactionReceived -> (f, s)
              TransactionSent -> (s, f)

      particpantObj :: Api.AddressAny -> Int32 -> Aeson.Value
      particpantObj addr balance =
        object [ "address" .= addr
               , "lovelace" .= balance
               ]

instance Aeson.FromJSON DetailedStatus where
  parseJSON = Aeson.withObject "Detailed Status" $ \o -> do
    [fp, sp] <- o .: "participants"
    firstAddr <- fp .: "address"
    firstBalance <- fp .: "lovelace"
    secondAddr <- sp .: "address"
    secondBalance <- sp.: "lovelace"
    txns <- o .: "transactions"
    ts <- fmap Map.fromList $ for txns $ \ti -> do
      tid <- ti .: "id"
      toAddr <- ti .: "to"
      amount <- ti .: "lovelace"
      time <- ti .: "time"
      let
        dir = case toAddr == firstAddr of
          True -> TransactionReceived
          _ -> TransactionSent
      pure $ (tid, TransactionInfo tid time amount dir)
    pure $ DetailedStatus firstAddr secondAddr firstBalance secondBalance ts

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

instance Aeson.ToJSON InstanceResponse where
  toJSON = \case
    NewPaymentChannel name (FundInternalWallet addr payload) ->
      object [ mkTag "new"
             , "name" .= name
             , "address" .= addr
             , "tx" .= payload
             ]

    InstanceError errorMsg ->
      object [ mkTag "error"
             , "message" .= errorMsg
             ]

    LockAttempt name result ->
      object $ [ "name" .= name
               ] <>
               (case result of
                  LockFundInternal (FundInternalWallet addr payload) ->
                    [ mkTag "payToInternalWalletTx"
                    , "address" .= addr
                    , "tx" .= payload
                    ]
                  LockInChannel (FundPaymentChannel amount payload witnessPayload) ->
                    [ mkTag "lockTx"
                    , "lovelace" .= amount
                    , "tx" .= payload
                    , "witness" .= witnessPayload
                    ]
                  LockCreateOutput amount payload ->
                    [ mkTag "createOutputTx"
                    , "lovelace" .= amount
                    , "tx" .= payload
                    ]
               )

    SendTx name (SendTxRaw fromAddr toAddr amount payload) -> do
      object $ [ mkTag "sendTx"
               , "name" .= name
               , "from" .= fromAddr
               , "to" .= toAddr
               , "lovelace" .= amount
               , "tx" .= payload
               ]

    SuccessMessage msg -> do
      object [ mkTag "success"
             , "message" .= msg
             ]

    ChannelStatus name status mStatus ->
      object $ [ mkTag "status"
               , "name" .= name
               , "status" .= statusText status
               ] <>
               (case mStatus of
                  Just ds -> [ "details" .= ds]
                  Nothing -> [])
      where
        statusText :: PaymentChannelStatus -> T.Text
        statusText = \case
          PaymentChannelStatus_Submitting -> "submitting"
          PaymentChannelStatus_WaitingForAccept -> "waitingForAccept"
          PaymentChannelStatus_Opening -> "opening"
          PaymentChannelStatus_Open -> "open"
          PaymentChannelStatus_Closing -> "closing"
          PaymentChannelStatus_Error -> "error"
          PaymentChannelStatus_Done -> "closed"

instance Aeson.FromJSON InstanceResponse where
  parseJSON = Aeson.withObject "Instance Response" $ \o -> do
    tag :: T.Text <- o .: "tag"
    case tag of
      "new" -> do
        name <- o .: "name"
        addr <- o .: "address"
        payload <- o .: "tx"
        pure $ NewPaymentChannel name $ FundInternalWallet addr payload

      "error" -> do
        message <- o .: "message"
        pure $ InstanceError message

      "payToInternalWalletTx" -> do
        name <- o .: "name"
        addr <- o .: "address"
        payload <- o .: "tx"
        pure $ LockAttempt name $ LockFundInternal (FundInternalWallet addr payload)

      "lockTx" -> do
        name <- o .: "name"
        amount <- o .: "lovelace"
        payload <- o .: "tx"
        witnessPayload <- o .: "witness"
        pure $ LockAttempt name $ LockInChannel (FundPaymentChannel amount payload witnessPayload)

      "createOutputTx" -> do
        name <- o .: "name"
        amount <- o .: "lovelace"
        payload <- o .: "tx"
        pure $ LockAttempt name $ LockCreateOutput amount payload

      "sendTx" -> do
        name <- o .: "name"
        fromAddr <- o .: "from"
        toAddr <- o .: "to"
        amount <- o .: "lovelace"
        payload <- o .: "tx"
        pure $ SendTx name (SendTxRaw fromAddr toAddr amount payload)

      "success" -> do
        msg <- o .: "message"
        pure $ SuccessMessage msg

      "status" -> do
        name <- o .: "name"
        statusText <- o .: "status"
        mStatus <- o .:? "details"
        status <- parseStatus statusText
        pure $ ChannelStatus name status mStatus
        where
          parseStatus :: MonadFail m => T.Text -> m PaymentChannelStatus
          parseStatus = \case
            "submitting" -> pure $ PaymentChannelStatus_Submitting
            "waitingForAccept" -> pure $ PaymentChannelStatus_WaitingForAccept
            "opening" -> pure $ PaymentChannelStatus_Opening
            "open" -> pure $ PaymentChannelStatus_Open
            "closing" -> pure $ PaymentChannelStatus_Closing
            "error" -> pure $ PaymentChannelStatus_Error
            "closed" -> pure $ PaymentChannelStatus_Done
            x -> fail $ "Invalid status text: " <> T.unpack x

      x -> fail $ "Invalid tag: " <> T.unpack x

mkTag :: T.Text -> (Aeson.Key, Aeson.Value)
mkTag = ("tag" .=)

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
    PaymentChannelStatus_Done -> name <> " is closed"

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
