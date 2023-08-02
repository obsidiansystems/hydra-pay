{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Types where

import Data.Int
import Data.Text (Text)
import HydraPay.Utils
import Control.Lens
import qualified Cardano.Api as Api

newtype TxId =
  TxId { unTxId :: Text }
  deriving (Eq, Show)

data TxInput = TxInput
  { _txInput_hash :: TxId
  , _txInput_slot :: Int32
  }
  deriving (Eq, Show)

newtype ProxyAddress = ProxyAddress { unProxyAddress :: Api.AddressAny }
  deriving (Eq, Ord, Show)

class ToAddress a where
  toAddress :: a -> Api.AddressAny

instance ToAddress Api.AddressAny where
  toAddress = id

instance ToAddress ProxyAddress where
  toAddress = unProxyAddress

instance Api.HasTypeProxy ProxyAddress where
  data AsType ProxyAddress = AsAddressAny
  proxyToAsType _ = AsAddressAny

instance Api.SerialiseAddress ProxyAddress where
  serialiseAddress = Api.serialiseAddress . unProxyAddress
  deserialiseAddress _ t = fmap ProxyAddress $ Api.deserialiseAddress Api.AsAddressAny t

makeLenses ''TxInput

mkTxInput :: TxId -> Int32 -> TxInput
mkTxInput = TxInput

txInputToText :: TxInput -> Text
txInputToText (TxInput (TxId hash) slot) = hash <> "#" <> tShow slot
