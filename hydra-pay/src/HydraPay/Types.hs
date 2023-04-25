{-# LANGUAGE TemplateHaskell #-}

module HydraPay.Types where

import Data.Int
import Data.Text (Text)
import HydraPay.Utils
import Control.Lens

newtype TxId =
  TxId { unTxId :: Text }
  deriving (Eq, Show)

data TxInput = TxInput
  { _txInput_hash :: TxId
  , _txInput_slot :: Int32
  }
  deriving (Eq, Show)

makeLenses ''TxInput

mkTxInput :: TxId -> Int32 -> TxInput
mkTxInput = TxInput

txInputToText :: TxInput -> Text
txInputToText (TxInput (TxId hash) slot) = hash <> "#" <> tShow slot
