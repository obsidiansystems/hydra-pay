{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HydraPay.Cardano.Hydra.Api.ClientInput where

import Control.Lens
import Data.Map (Map)
import Data.Maybe (isJust)
import qualified Cardano.Api as Api
import GHC.Generics
import Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Aeson.KeyMap as KeyMap

data ClientInput
  = Init
  | Abort
  | Commit { utxo :: Value }
  | NewTx { transaction :: Value }
  | GetUTxO
  | Close
  | Contest
  | Fanout
  deriving (Generic, Show, Eq)

instance FromJSON ClientInput
instance ToJSON ClientInput

data TxOutWithWitness = TxOutWithWitness
  { txOut :: Api.TxOut Api.CtxUTxO Api.BabbageEra
  , witness :: Maybe ()
  -- We aren't bothering with script info, we don't send any
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON TxOutWithWitness where
  toJSON TxOutWithWitness{txOut, witness} =
    case toJSON txOut of
      Object km
        | isJust witness ->
            Object $ km & "witness" `KeyMap.insert` toJSON witness
      x -> x

instance FromJSON TxOutWithWitness where
  parseJSON v = do
    txOut <- parseJSON v
    flip (withObject "TxOutWithWitness") v $ \o -> do
      witness <- o .:? "witness"
      pure $ TxOutWithWitness{txOut, witness}


-- type UTxO = UTxO' (Api.TxOut Api.CtxUTxO Api.BabbageEra)

-- | Newtype with phantom types mostly required to work around the poor interface
-- of 'Ledger.UTXO' and provide 'Monoid' and 'Foldable' instances to make utxo
-- manipulation bareable.
newtype UTxO' out = UTxO
  { toMap :: Map Api.TxIn out
  }
  deriving newtype
    ( Eq
    , Show
    , Functor
    , Foldable
    , Semigroup
    , Monoid
    , ToJSON
    , FromJSON
    )

newtype DraftCommitTxRequest = DraftCommitTxRequest
  { utxoToCommit :: UTxO' TxOutWithWitness }
  -- ^ Ensure we encode a "tag"
  deriving stock (Eq, Show, Generic)
  deriving newtype (ToJSON, FromJSON)

newtype DraftCommitTxResponse = DraftCommitTxResponse { commitTx :: Text }
  deriving (Generic)

instance FromJSON DraftCommitTxResponse where
  parseJSON = withObject "DraftCommitTxResponse" $ \o -> do
    hex <- o .: "cborHex"
    pure $ DraftCommitTxResponse hex

massageUtxo :: Api.UTxO Api.BabbageEra -> UTxO' TxOutWithWitness
massageUtxo utxo =
  UTxO $ fmap (flip TxOutWithWitness Nothing) $ Api.unUTxO utxo
